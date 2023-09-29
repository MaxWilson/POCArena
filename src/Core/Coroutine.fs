module Coroutine

type ExecutionResult<'actionOut, 'feedback, 'ctx, 'finalResult> = Finished of 'finalResult | AwaitingAction of 'actionOut * Behavior<'actionOut, 'feedback, 'ctx, 'finalResult>
and Behavior<'actionOut, 'feedback, 'ctx, 'finalResult> = 'feedback * 'ctx -> ExecutionResult<'actionOut, 'feedback, 'ctx, 'finalResult>
and RunChildRequest<'actionOut, 'feedback, 'ctx, 'finalResult> = RunChildRequest of Behavior<'actionOut, 'feedback, 'ctx, 'finalResult>
type ReturnAction<'actionOut> = ReturnAction of 'actionOut
type QueryRequest<'ctx, 'result> = QueryRequest of ('ctx -> 'result)
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
            match r with
            | AwaitingAction(action, _) ->
                AwaitingAction(action, binder r) // See below, but this is effectively Action: Attack(followup: cowardly justAttack)
            | Finished result ->
                binder r (feedback, ctx) // yes, we're re-using the feedback. That might be a mistake.
    (*
        Okay, let's think through this scenario from the perspective of the behavior which is USING the child behavior.

        ignoredFeedback, ctx1
            parent behavior: Cowardly
                child behavior: justAttack
                    Action: Attack(followup: justAttack)
            Action: Attack(followup: cowardly justAttack)
        Attack feedback, ctx2
            parent behavior: Cowardly
                child behavior: justAttack
                    Action: Attack(followup: justAttack)
            Action: Attack(followup: cowardly justAttack)
        Attack feedback, ctx3
            parent behavior
                child behavior: flee
                    Action: Move

    *)
let behavior = BehaviorBuilder()
