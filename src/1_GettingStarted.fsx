
let amp amount input = input * amount

let limit threshold input  =
    if input > threshold then threshold
    else if input < -threshold then -threshold
    else input

let lowPass frq input = input // just a dummy - for now...

let fadeIn stepSize input = input // just a dummy - for now...

let mix abRatio a b = a * abRatio + b * (1.0 - abRatio)

let distort drive input =
    let amplified = amp drive input
    let limited = limit 1.0 amplified
    limited



    // assign each output value to a symbol
    let blendedDistortion drive input =
        let amped = input |> amp drive
        let hardLimited = amped |> limit 0.7
        let softLimited = amped |> lowPass 0.2
        let mixed = mix 0.5 hardLimited softLimited
        let fadedIn = mixed |> fadeIn 0.1
        let gained = amp 0.5 fadedIn
        gained
            
        
    
    

// or: use pipe style whenever possible
let blendedDistortion_Alt2 drive input =
    let amped = input |> amp drive

    (
        amped |> limit 0.7,
        amped |> lowPass 0.2
    )
    ||> mix 0.5
    |> fadeIn 0.1
    |> amp 0.5


// Test: each value gets transformed
[ 0.1; 0.3; 0.5; 0.2; 0.8; 1.0; 0.5; 0.0 ]
|> List.map (blendedDistortion_Alt2 2.0)

// Problem:  fadeIn and lowPass



//
//
//List.map (fun x -> x + 1) [ 1; 2; 3; ]
//
//[ 1; 2; 3; ] |> List.map (fun x -> x + 1)
//
//let addOneToAll = List.map (fun x -> x + 1)
//addOneToAll [ 1; 2; 3; ]
//
//
