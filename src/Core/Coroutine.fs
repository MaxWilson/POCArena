module Coroutine

type ReturnAction<'actionOut> = ReturnAction of 'actionOut
type QueryRequest<'ctx, 'result> = QueryRequest of ('ctx -> 'result)
type YieldRequest = class end // Yield means "don't quit behavior but there's nothing more to do until context changes"
type ExecutionResult<'actionOut, 'feedback, 'ctx, 'finalResult> = Finished of 'finalResult | AwaitingAction of 'actionOut * Behavior<'actionOut, 'feedback, 'ctx, 'finalResult>
and Behavior<'actionOut, 'feedback, 'ctx, 'finalResult> = 'feedback * 'ctx -> ExecutionResult<'actionOut, 'feedback, 'ctx, 'finalResult>
and RunChildRequest<'actionOut, 'feedback, 'ctx, 'finalResult> = RunChildRequest of Behavior<'actionOut, 'feedback, 'ctx, 'finalResult>
let run logic (feedback, ctx) = logic(feedback, ctx)

type BehaviorBuilder() =
    member this.Return (x: 't) : Behavior<_,_,_,_> = fun (feedback, ctx) -> Finished x
    member this.ReturnFrom (x: Behavior<_,_,_,_>) = x
    // member this.Bind(b, f) = bind b f
    member this.Bind(ReturnAction(action), binder: _ -> Behavior<_,_,_,_>): Behavior<_,_,_,_> =
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
    member this.Bind(q: QueryRequest<_,'result>, binder: 'result -> Behavior<_,_,_,_>) =
        fun(feedback, ctx) ->
            let (QueryRequest qf) = q
            let r = qf ctx
            run (binder r) (feedback, ctx)
    member this.Bind(RunChildRequest(lhs: Behavior<_,_,_,_>), binder: ExecutionResult<_,_,_,_> -> Behavior<_,_,_,_>): Behavior<_,_,_,_> =
        fun(feedback, ctx) ->
            let r: ExecutionResult<_,_,_,_> = run lhs (feedback, ctx)
            binder r // we DON'T want to process the output before "returning" it back to the computation expression, because it might want to short circuit at a higher level, e.g. for cowardly
let behavior = BehaviorBuilder()
