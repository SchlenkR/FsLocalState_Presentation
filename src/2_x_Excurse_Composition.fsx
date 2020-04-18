
let amp amount input = input * amount

let lowPass frq input = input // just a dummy - for now...

let fadeIn stepSize input = input // just a dummy - for now...

let myEffect input =
    input
    |> amp 2.0
    |> fadeIn 0.1
    |> lowPass 8000.0

// the power of composition:
// - the |> operator is a composition operator that pipes a value into a function that
// - evaluates to a values that can again be piped into a function that
// - evaluates to a values that can again be piped into a function that
// - evaluates to a values that can again be piped into a function that
// - evaluates to a values that can again be piped into a function that


let pipe x f = f x
let ( |-> ) = pipe

let myEffect2 input =
    input
    |-> amp 2.0
    |-> fadeIn 0.1
    |-> lowPass 8000.0


// it works!
let input = [ 0.1; 0.3; 0.5; 0.2; 0.8; 1.0; 0.5; 0.0  ] 
let res1 = input |> List.map myEffect
let res2 = input |> List.map myEffect2

res1 = res2
