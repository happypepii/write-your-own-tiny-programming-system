type Expression =
    | Constant of int
    | Binary of char * Expression * Expression

let rec evaluate e= 
    match e with
    | Constant(n) -> n
    | Binary(op, e1, e2) ->
    let n1 = evaluate e1
    let n2 = evaluate e2
    match op with
    | '+' -> n1 + n2
    | '*' -> n1 * n2
    | _ -> failwith "unknown operator"



// 2 + (4 * 10)
let n = Binary('+', Constant(2), 
    Binary(
        '*', Constant(4), Constant(10)
))

evaluate n

