module BlockMonad =

    type BlockOutput<'value, 'state> =
        { value: 'value
          state: 'state }

    type Block<'value, 'state> = Block of ('state option -> BlockOutput<'value, 'state>) with
        member this.Get() = let (Block block) = this in block
        member this.Execute(state) = this.Get() state

    let bind (currentBlock: Block<'valueA, 'stateA>) (rest: 'valueA -> Block<'valueB, 'stateB>): Block<'valueB, 'stateA * 'stateB> =
        Block <| fun previousStatePack ->

            // Deconstruct state pack:
            // state is a tuple of: ('stateA * 'stateB) option
            // that gets transformed to: 'stateA option * 'stateB option
            let previousStateOfCurrentBlock, previousStateOfNextBlock =
                match previousStatePack with
                | None -> None, None
                | Some(stateA, stateB) -> Some stateA, Some stateB

            // no modifications from here:
            // previousStateOfCurrentBlock and previousStateOfNextBlock are now
            // both optional, but block who use it can deal with that.

            // The result of currentBlock is made up of an actual value and a state that
            // has to be "recorded" by packing it together with the state of the
            // next block.
            let currentBlockOutput = currentBlock.Execute previousStateOfCurrentBlock

            // Continue evaluating the computation:
            // passing the actual output value of currentBlock to the rest of the computation
            // gives us access to the next block in the computation:
            let nextBlock = rest currentBlockOutput.value

            // Evaluate the next block and build up the result of this bind function
            // as a block, so that it can be used as a bindable element itself -
            // but this time with state of 2 blocks packed together.
            let nextBlockOutput = nextBlock.Execute previousStateOfNextBlock
            
            { value = nextBlockOutput.value
              state = currentBlockOutput.state, nextBlockOutput.state }

    let (>>=) = bind

    let returnB x =
        Block <| fun unusedState ->
            { value = x
              state = () }

    type BlockBuilder() =
        member this.Bind(block, rest) = bind block rest
        member this.Return(x) = returnB x
        member this.ReturnFrom(x): Block<_, _> = x

    let block = BlockBuilder()

    
    [<AutoOpen>]
    module Feedback =
            
        type Feedback<'fbdValue, 'value> = { feedback: 'fbdValue; out: 'value }

        let (<->) seed (f: 'fbdValue -> Block<Feedback<'fbdValue,'value>,'state>) =
            Block <| fun prev ->
                let myPrev,innerPrev = 
                    match prev with
                    | None            -> seed,None
                    | Some (my,inner) -> my,inner
                let fRes = f myPrev
                let lRes = fRes.Execute innerPrev
                let feed = lRes.value
                let innerState = lRes.state
                { value = feed.out; state = feed.feedback,Some innerState }


    [<AutoOpen>]
    module Evaluation =
        
        /// Creates a stateful evaluator that emits values and state.
        let createEvaluatorWithStateAndValues (blockWithInput: 'vIn -> Block<'vOut,'s>) =
            let mutable state = None
            fun inputValues ->
                seq {
                    for i in inputValues ->
                        let block = blockWithInput i
                        let result = block.Execute state
                        state <- Some result.state
                        result
                }

        /// Creates a stateful evaluator that emits values only.
        let createEvaluatorWithValues (blockWithInput: 'vIn -> Block<'vOut,'s>) =
            let stateAndValueEvaluator = createEvaluatorWithStateAndValues blockWithInput
            fun inputValues ->
                stateAndValueEvaluator inputValues
                |> Seq.map (fun stateAndValue -> stateAndValue.value)

        /// evaluates a processing function 10 times.
        let evaluate f =
            let evaluateWithValues = f |> createEvaluatorWithValues
            evaluateWithValues (Seq.init 10 id) |> Seq.toList

        /// Evaluates a 'generator' function - a function with no input - 10 times.
        let evaluateGen f = (fun _ -> f) |> evaluate


module Blocks =
    
    open BlockMonad

    let amp (factor: float) (i: float): float = i * factor

    let limit threshold i: float =
        if i > threshold then threshold
        else if i < -threshold then -threshold
        else i

    let mix amount a b: float = b * amount + a * (1.0 - amount)

    let lowPass timeConstant input =
        Block <| fun state ->
            let state = Option.defaultValue 0.0 state

            let diff = state - input
            let out = state - diff * timeConstant
            let newState = out
            
            { value = out
              state = newState }

    let fadeIn stepSize initial (input: float) =
        Block <| fun state ->
            let state = Option.defaultValue initial state

            let result = input * state
            let newState = min (state + stepSize) 1.0

            { value = result
              state = newState }


module ``UseCase 1: Two counter alternatives`` =
    
    open BlockMonad

    // some simple blocks
    let counter (seed: float) (increment: float) =
        Block <| fun maybeState ->
            let state = Option.defaultValue seed maybeState
            let res = state + increment
            {value=res; state=res}

    // we can rewrite 'counter' by using feedback:
    let counterAlt (seed: float) (increment: float) =
        seed <-> fun state ->
            block {
                let res = state + increment
                return { out=res; feedback=res }
            }

    let evaluatedCounter = counter 0.0 1.5 |> evaluateGen
    // evaluates to: [1.5; 3.0; 4.5; 6.0; 7.5; 9.0; 10.5; 12.0; 13.5; 15.0]

    let evaluatedCounterAlt = counterAlt 0.0 1.5 |> evaluateGen
    // evaluates to: [1.5; 3.0; 4.5; 6.0; 7.5; 9.0; 10.5; 12.0; 13.5; 15.0]

module ``UseCase 2: State in 'block' syntax`` =
    
    open BlockMonad
    open Blocks

    let myFx input =
        block {
            // I would like to feed back the amped value
            // and access it in the next cycly
            // - but how?
            let amped = amp 0.5 input (* - (lastAmped * 0.1) *)
            let! lp = lowPass 0.2 amped
            return lp
        }

    let myFxWithFeedback input =
        // initial value for lastAmped is: 0.0
        0.0 <-> fun lastAmped ->
            block {
                let amped = amp 0.5 input - (lastAmped * 0.1)
                let! lp = lowPass 0.2 amped
                // we emit our actual value (lp), and the feedback value (amped)
                return { out=lp; feedback=amped }
            }
