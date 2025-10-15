// The cook function takes two parameters: recipe and ingredient.
// In F#, function arguments are evaluated before the function itself is called.
// Therefore, the expressions inside the arguments are executed first.
//
// Execution order:
// 1. Evaluate the first argument → prints "Which recipe?" and returns "Curry"
// 2. Evaluate the second argument → prints "Which ingredient?" and returns "Rice"
// 3. Finally, call cook "Curry" "Rice"
//
// Output:
// Which recipe?
// Which ingredient?
// Cooking Curry with Rice
let cook recipe ingredient =
    printfn "Cooking %s with %s" recipe ingredient

cook (printfn "Which recipe?"; "Curry") (printfn "Which ingredient?"; "Rice")

// The function `iff` takes three parameters:
//   cond → a boolean condition
//   tbranch → the expression to evaluate if cond is true
//   fbranch → the expression to evaluate if cond is false
//
// When calling `iff`, F# evaluates all arguments *before* the function body is executed.
// This behavior is called "eager evaluation".
//
// Therefore:
// 1. F# first evaluates (input % 3 = 0) → returns true
// 2. Then it evaluates (printfn "Divisible by 3!") → prints "Divisible by 3!" and returns unit
// 3. Then it evaluates (printfn "Not divisible by 3!") → prints "Not divisible by 3!" and returns unit
// 4. Finally, it calls iff true () (), but both print statements have already been executed.
//
// Output:
// Divisible by 3!
// Not divisible by 3!

let iff cond tbranch fbranch =
  if cond then tbranch else fbranch

let n = 6

iff (n % 3 = 0) 
  (printfn "Divisible by 3!")
  (printfn "Not divisible by 3!")


// --------------------------------------------------------
// Lazy conditional
// --------------------------------------------------------

let ifff cond (tbranch:Lazy<_>) (fbranch:Lazy<_>) = 
  if cond then tbranch.Value else fbranch.Value

let input = 6

ifff (input % 3 = 0) 
  (lazy printfn "Divisible by 3!")
  (lazy printfn "Not divisible by 3!")

// --------------------------------------------------------
// Lazy list implementation
// --------------------------------------------------------

type LazyList = 
  | Cons of int * Lazy<LazyList>
  | End

let rec printList count l = 
  if count > 0 then
    match l with 
    | End -> ()
    | Cons(l, ll) ->
        printfn "%d" l
        printList (count-1) ll.Value

let l1 = Cons(1, lazy Cons(2, lazy End))
let rec nums n = Cons(n, lazy nums (n+1))
let rec l2 = Cons(42, lazy l2)

printList 10 l1
printList 100 l2
printList 1000 (nums 0)

