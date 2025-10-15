// --------------------------------------------------------
// Operators using Map
// --------------------------------------------------------

// version 1
let op1 = Map.empty
let op2= Map.add '+' (fun a b -> a + b) op1
let op3= Map.add '*' (fun a b -> a * b) op2

// version 2
let ops = Map.ofList [
    '+', (fun a b -> a + b)
    '*', (fun a b -> a * b)
]

// version 3
let opss = Map.ofList [
    '+', (+)
    '*', (*)
]

type Expression =
    | Constant of int
    | Binary of char * Expression * Expression

let rec evaluate e= 
    match e with
    | Constant(n) -> n
    | Binary(op, e1, e2) ->
    let n1 = evaluate e1
    let n2 = evaluate e2
    match opss.TryFind op with
    | Some f -> f n1 n2
    | _ -> failwith "unknown operator"



ops['+'] 45 10 = opss['+'] 45 10

// 2 + (4 * 10)
let n = Binary('+', Constant(2), 
    Binary(
        '*', Constant(4), Constant(10)
))

evaluate n