type Expression =
    | Constant of int
    | Binary of string * Expression * Expression
    | Variable of string

let rec evaluation (ctx:Map<string, int>) expr =
    match expr with
        | Constant n -> n
        | Binary(op, e1, e2) -> 
            let v1 = evaluation ctx e1
            let v2 = evaluation ctx e2
            match op with
            | "+" -> v1 + v2
            | "*" -> v1 * v2 
            |_ -> failwith "unsupported binary operator"
        | Variable key ->
            match ctx.TryFind key with
            | Some v -> v
            |_ -> failwith "variable not in the map"
// 表達式: (x * 3) + y
let e =
  Binary("+",
    Binary("*", Variable("x"), Constant(3)),
    Variable("y"))

let ctx =
  Map.ofList [("x", 50); ("y", 2)]

// 應該是 (4 * 3) + 2 = 14
evaluation ctx e
