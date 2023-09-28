module Coroutine

type 't Feedback = Success of 't | Failure of 't
type ReturnAction<'actionOut> = ReturnAction of 'actionOut
type QueryRequest<'ctx, 'result> = QueryRequest of ('ctx -> 'result)
type ExecutionResult<'actionOut, 'feedback, 'ctx> = Finished of 'feedback Feedback | Yield | AwaitingAction of 'actionOut * Behavior<'actionOut, 'feedback, 'ctx>
and Behavior<'actionOut, 'feedback, 'ctx> = 'feedback Feedback  * 'ctx -> ExecutionResult<'actionOut, 'feedback, 'ctx>

let run logic (feedback, ctx) = logic(feedback, ctx)
// let bind (lhs: Coroutine<'action, 'memory, 'context>) (binder: _ -> Coroutine<'action,_,'context>) : Coroutine<'action, _, 'context> =
//     fun (feedback, ctx) ->
//         match run lhs (feedback, ctx) with
//         | (Finished _ | Yield) as result -> result
//         | AwaitingAction(a, continuation) -> AwaitingAction(a, run continuation |> Coroutine)
//     |> Coroutine

type BehaviorBuilder() =
    member this.Return (x: 't Feedback) : Behavior<_,_,_> = fun (feedback, ctx) -> Finished x
    member this.ReturnFrom (x: Behavior<_,_,_>) = x
    // member this.Bind(b, f) = bind b f
    member this.Bind(ReturnAction(action), binder: _ -> Behavior<_,_,_>): Behavior<_,_,_> =
        fun (feedback, context) ->
            let followupRoutine = binder (feedback, context)
            (* consider a block of behavior that looks like this:

               let! feedback, context = ReturnAction(SimpleAttack)
               return if feedback > 0 then Success else Failure

               From the caller's perspective, this looks like two separate, nested behaviors

               let AwaitingAction(action1, continuation) = behavior(...) // this is the ReturnAction
               let Finished Failure = continuation (feedback, context) // Here's where we consume feedback and evaluate it to produce a result.

               So even though it LOOKS at first like it's weird for feedback and context to get fed to followupBehavior, it actually makes sense
               because mem and action are outputs whereas feedback and context are inputs.
            *)
            // previously, // we discard the action/memory/context here, but we might have used them previously via QueryRequest to construct the action we're requesting
            AwaitingAction(action, run followupRoutine)
    member this.Bind(b: QueryRequest<_,'result>, binder: 'result -> Behavior<_,_,_>) =
        fun(feedback, ctx) ->
            let (QueryRequest qf) = b
            let r = qf ctx
            run (binder r) (feedback, ctx)
    // we might not ever need this next guy
    // member this.Bind(lhs: Coroutine<_,_,_>, binder: _ -> Coroutine<_,_,_>) =
        // fun (feedback, ctx) ->
        //     match run lhs (feedback, ctx) with
        //     | (Finished _ | Yield) as result -> result
        //     | AwaitingAction(a, continuation) ->
        //         let continuation = continuation
        //         AwaitingAction(a, run continuation |> Coroutine)
        // |> Coroutine
let behavior = BehaviorBuilder()

let tryMultipleBindings() = behavior {
    let! (feedback, ctx) = ReturnAction 1
    let! (feedback2, ctx) = ReturnAction 1
    let! (feedback3, ctx) = ReturnAction 1
    let getSuccess = function Success x -> x | _ -> shouldntHappen()
    return Success ([feedback; feedback2; feedback3] |> List.map getSuccess |> List.max)
    }

let tryMultipleBindings2() =
    let b = behavior
    let x = b.Return(Success ([1;2;3] |> List.max))
    let y: Behavior<_,_,_> = b.Bind(ReturnAction 1, fun (feedback3, ctx) -> x)
    let z: Behavior<_,_,_> = b.Bind(ReturnAction 1, fun (feedback3, ctx) -> y)
    let zz: Behavior<_,_,_> = b.Bind(ReturnAction 1, fun (feedback3, ctx) -> z)
    let getSuccess = function Success x -> x | _ -> shouldntHappen()
    b.Bind(ReturnAction 1, fun (feedback, ctx) ->
        b.Bind(ReturnAction 1, fun (feedback2, ctx) ->
            b.Bind(ReturnAction 1, fun (feedback3, ctx) ->
                b.Return(Success ([feedback; feedback2; feedback3] |> List.map getSuccess |> List.max))
            )
        )
    )