
#load "1_GettingStarted.fsx"
open ``1_GettingStarted``

let lowPassCtor() =
    let mutable lastOut = 0.0
    fun timeConstant input ->
        let diff = lastOut - input
        lastOut <- lastOut - diff * timeConstant
        lastOut

let fadeInCtor() =
    let mutable lastValue = 0.0
    fun stepSize input ->
        let result = input * lastValue
        lastValue <- min (lastValue + stepSize) 1.0
        result



// that compiles, but doesn't work.    
let blendedDistortion drive input =

    let amped = input |> amp drive

    (
        amped |> limit 0.7,
        amped |> lowPassCtor() 0.2   // we would like to use lowPassCtor
    )
    ||> mix 0.5
    |> fadeInCtor() 0.1              // we would like to use fadeInCtor
    |> amp 0.5



// this works, but we have to keep track of our objects
let blendedDistortionCtor() =

    // create and hold references to stateful objects
    let lowPassInstance = lowPassCtor()
    let fadeInInstance = fadeInCtor()

    fun drive input ->
        let amped = input |> amp drive
        let hardLimited = amped |> limit 0.7
        let softLimited = amped |> lowPassInstance 8000.0
        let mixed = mix 0.5 hardLimited softLimited
        let fadedIn = mixed |> fadeInInstance 0.1
        let gained = amp 0.5 fadedIn
        gained

// again, create and hold references to stateful objects
let myEffect = (blendedDistortionCtor()) 2.0


[ 0.1; 0.3; 0.5; 0.2; 0.8; 1.0; 0.5; 0.0  ]
|> List.map myEffect


// that's not what we want! we want to have functions that can be composed like functions
// without keeping track of references.
