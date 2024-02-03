type Val =
  | Num of int
  | Closure of string * Expr * Ctx
  | VProd of Lazy<Eval<Val>> * Lazy<Eval<Val>>
  | VInj of bool * Lazy<Eval<Val>>

and Expr =
  | Const of int
  | Var of string
  | Unary of string * Expr
  | Binary of string * Expr * Expr
  | If of Expr * Expr * Expr
  | Func of string * Expr
  | App of Expr * Expr
  | Prod of Expr * Expr
  | Proj of bool * Expr
  | Inj of bool * Expr
  | Match of Expr * string * Expr * Expr
  | LetRec of string * Expr * Expr

and Ctx =
  Map<string, Lazy<Eval<Val>>>

and Eval<'T> =
  | Ok of 'T
  | Error of string

module Eval =
  let bind f e =
    match e with
    | Ok(o) -> f o
    | Error(s) -> Error(s)
  let result v =
    Ok(v)

type EvalBuilder() =
  member x.Bind(a, f) = Eval.bind f a
  member x.Return v = Eval.result v
  member x.ReturnFrom v = v

let evalM = new EvalBuilder()

let lookup (ctx: Ctx) id =
  evalM {
    match ctx.TryFind id with
    | Some(v) -> return v
    | None -> return! Error("Undefined Variable")
  }

let neg (v:Val) =
  match v with
  | Num(n) -> Ok(Num(-n))
  | _ -> Error("'-' can only be applied on numbers")

let binop op (v1:Val) (v2: Val) =
  match v1, v2 with
  | Num(n1), Num(n2) -> match op with
                        | "+" -> Ok(Num(n1 + n2))
                        | "*" -> Ok(Num(n1 * n2))
                        | _ -> Error("Unsupported binary operation")
  | _ -> Error("'+' or '*' can only be applied on numbers")

let force (v:Lazy<'a>) = v.Value

let extend (m1: Map<_,_>) (m2: Map<_,_>) =
  Map.fold (fun (s: Map<_,_>) k v -> s.Add(k, v)) m1 m2

let rec eval (ctx: Ctx) e =
  match e with
  | Const(n) -> lazy evalM { return Num(n) }
  | Var(id) -> lazy evalM {
                 let! v = lookup ctx id
                 return! v.Value
               }
  | Unary(op, e) -> lazy evalM {
                      let! v = force (eval ctx e)
                      match op with
                      | "-" -> let! res = neg v
                               return res
                      | _ -> return! Error("Unsupported unary operation")
                    }
  | Binary(op, e1, e2) -> lazy evalM {
                            let! v1 = force (eval ctx e1)
                            let! v2 = force (eval ctx e2)
                            let! res = binop op v1 v2
                            return res
                          }
  | If(e, e1, e2) -> lazy evalM {
                            let b = force (eval ctx e)
                            match! b with
                            | Num(1) -> return! force (eval ctx e1)
                            | Num(_) -> return! force (eval ctx e2)
                            | _ -> return! Error("Can only branch on numbers")
                          }
  | Func(s, e) -> lazy Ok(Closure(s, e, ctx))
  | App(f, e) -> lazy evalM {
                   match! force (eval ctx f) with
                   | Closure(s, body, cap) ->
                      let v = eval ctx e
                      return! force (eval ((extend ctx cap).Add(s, v)) body)
                   | _ -> return! Error("Only closures can be applied")
                 }
  | Prod(e1, e2) -> lazy Ok(VProd(lazy (eval ctx e1).Value, lazy (eval ctx e2).Value))
  | Proj(b, p) -> lazy evalM {
                         match! force (eval ctx p) with
                         | VProd(v1, v2) -> if b then return! v1.Value else return! v2.Value
                         | _ -> return! Error("Can only project from tuples")
                       }
  | Inj(b, e) -> lazy Ok(VInj(b, lazy (eval ctx e).Value))
  | Match(e, id, e1, e2) -> lazy evalM {
                                   match! force (eval ctx e) with
                                   | VInj(b, v) -> let ctx' = ctx.Add(id, v)
                                                   if b then return! force (eval ctx' e1) else return! force (eval ctx' e2)
                                   | _ -> return! Error("Can only match on injections")
                                 }
  | LetRec(id, e1, e2) -> lazy evalM {
                                 let rec ctx' = ctx.Add(id, lazy (eval ctx' e1).Value)
                                 return! force (eval ctx' e2)
                               }

let fact =
  LetRec("fact",
    Func("x", If(
      Var("x"),
      Const(1),
      Binary(
        "*", Var("x"),
        App(Var("fact"), Binary("+", Const(-1), Var("x")))
      )
    )),
    App(Var("fact"), Const(5))
  )

match (eval Map.empty fact).Value with
| Ok(Num(n)) -> printfn "%d" n
| Ok(_) -> printfn "Oops"
| Error(e) -> failwith e

let lazyBoom =
  Proj(true, Prod(Const(1), Proj(false, Inj(false, Const(1)))))

match (eval Map.empty lazyBoom).Value with
| Ok(Num(1)) -> printfn "lazyBoon ok"
| Ok(_) -> printfn "Oops"
| Error(e) -> failwith "Not lazy at all"
