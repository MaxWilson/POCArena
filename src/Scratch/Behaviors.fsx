#I ".."
#load "Core/Common.fs"

let enemies = ["Bob", 10; "Joe", 14; "Sue", 6] |> Map.ofList

type Feedback = Success | Failure
type ExecutionResult<'actionOut, 'memory, 'ctx> = Finished of Feedback | Yield | AwaitingAction of 'actionOut * 'memory * Behavior<'actionOut, 'memory, 'ctx>
and Behavior<'actionOut, 'memory, 'ctx> = Feedback * 'memory * 'ctx -> ExecutionResult<'actionOut, 'memory, 'ctx>
type ReturnAction<'actionOut, 'memory> = ReturnAction of 'actionOut * 'memory
type QueryRequest<'memory, 'ctx, 'result> = QueryRequest of ('memory * 'ctx -> 'result)
let run (bhv: Behavior<_,_,_>) (a,m,c) = bhv(a,m,c)
let bind (lhs: Behavior<'action, 'memory, 'context>) (binder: _ -> Behavior<'action,_,'context>) : Behavior<'action, _, 'context> =
    fun (a, m, c) ->
        match run lhs (a, m, c) with
        | (Finished _ | Yield) as result -> result
        | AwaitingAction(a, m', continuation) -> AwaitingAction(a, m', run continuation)

type BehaviorBuilder() =
    member this.Return (x: Feedback) = fun _ -> Finished x
    member this.ReturnFrom (x: Behavior<_,_,_>) = x
    // member this.Bind(b, f) = bind b f
    member this.Bind(ReturnAction(action, mem), binder: _ -> Behavior<_,_,_>) =
        fun (feedback, memory0, context0) ->
            let followupBehavior = binder (feedback, memory0, context0) // What makes this tricky is that we will need two sets of arguments to get a final result
            (* consider a block of behavior that looks like this:

               let! feedback, memory, context = ReturnAction(SimpleAttack, mem)
               return if feedback > 0 then Success else Failure

               From the caller's perspective, this looks like two separate, nested behaviors

               let AwaitingAction(action1, memory1, continuation) = behavior(...) // this is the ReturnAction
               let Finished Failure = continuation (feedback, memory1, context) // Here's where we consume feedback and evaluate it to produce a result.

               So even though it LOOKS at first like it's weird for memory0 to get fed to followupBehavior, it actually makes sense
               because mem and action are outputs whereas feedback and memory0 are inputs.
            *)
            // previously, // we discard the action/memory/context here, but we might have used them previously via QueryRequest to construct the action we're requesting
            AwaitingAction(action, mem, run followupBehavior)
    // member this.Bind(b: QueryRequest<_,_,'result>, f: 'result -> Behavior<_,_,_>) =
    //     fun(a, m, c) ->
    //         let (QueryRequest qf) = b
    //         let r = qf(m,c)
    //         (f r)(a,m,c)
    // member this.Delay f = f
    // member this.Delay(f): Behavior<_,_,_> = fun(action, memory, context) -> f()(action, memory, context)
    // member this.Zero() = fun _ -> None
    // member this.Run(b, s, m) = b(s, m)
let behavior = BehaviorBuilder()

type SimpleAction = Attack of name: string | MoveTo of name: string
// type Action = class end // make these concrete at first just to make dev easier
type Context = {
    enemyHP: Map<string, int>
    }
type Memory = class end
// In order to make this work, action and ctx have to be fixed for the whole tree, while memory has to be lensed.
let attack (name: string) = Attack name
type AttackMemory = {
    target: string option
    }
    with static member fresh = { target = None }

let trivial: Behavior<_,_,_> = behavior {
    return Failure
    }
let trivial0 =
    let b = behavior
    let x = b.Return(Failure)
    x
    // delay is not helpful here, since f just winds up argless and wrapped. Doesn't help propagate action/memory/context down into x. It's just a delay.
    // let f = fun() -> x
    // let d = b.Delay f
    // b.Delay(fun() -> b.Return(Failure))
let almosttrivial0(): Behavior<_,_,_> =
    let b = behavior
    b.Bind(ReturnAction(attack "Bob", notImpl()), fun (result: Feedback, mem: Memory, ctx: Context) -> b.Return(Failure))
// let almosttrivialB0(): Behavior<_,_,_> =
//     let b = behavior
//     b.Delay(fun() -> b.Bind(ActionRequest(attack "Bob", notImpl()), fun (result: ActionResult, mem: Memory, ctx: Context) -> b.Return(Failure)))

let almosttrivial(): Behavior<_,_,_> = behavior {
    let! (result: Feedback, mem: Memory, ctx: Context) = ReturnAction(attack "Bob", notImpl())
    return Failure
    }
let almosttrivial2(): Behavior<_,_,_> =
    let b = behavior
    let bind(ReturnAction(action, mem), bhv: (Feedback * 'memory * 'context -> ExecutionResult<_,_,_>)) =
        fun (action, mem', ctx) -> AwaitingAction(action, mem, bhv)
    bind(ReturnAction(attack "Bob", notImpl()), fun (result, mem, ctx) -> b.Return Failure) // is this the root of the problem here? If result/mem/ctx were already on b.Return Failure it would be perfect. bind(..., b.Return Failure) would work.
    // if we take a cue from the state monad, https://dev.to/shimmer/the-state-monad-in-f-3ik0,
    // we should be accepting a binder as rhs and not a behavior directly:
    (*
        /// ('a -> Stateful<'state, 'b>) -> Stateful<'state, 'a> -> Stateful<'state, 'b>
        let bind binder stateful =
            Stateful (fun state ->
                let result, state' = stateful |> run state
                binder result |> run state')
        *)
    // notice how the pattern is Wrap(run ==> binder ==> run), and the "let" binding is occuring in binder. What's the equivalent for behavior?

let quasitrivial = behavior {
    let! (result: Feedback, mem: Memory, ctx: Context) = ReturnAction(attack "Bob", notImpl())
    let! (result: Feedback, mem: Memory, ctx: Context) = ReturnAction(attack "Bob", notImpl())
    return Failure
    }
let quasitrivial1 =
    let b = behavior
    b.Bind(
        ReturnAction(attack "Bob", notImpl()),
        fun (result: Feedback, mem: Memory, ctx: Context) ->
            b.Bind(
                ReturnAction(attack "Bob", notImpl()),
                fun (result: Feedback, mem: Memory, ctx: Context) ->
                    b.Return(Failure)
                )
            )

let rec kill(): Behavior<_,_,_> = behavior {
    let! target = QueryRequest <| fun (mem: AttackMemory, ctx: Context) -> mem.target |> Option.orElseWith (fun () -> ctx.enemyHP |> Map.tryPick (fun name hp -> if hp > 0 then Some name else None))
    match target with
    | None -> return Success
    | Some target ->
        let mem = notImpl "attacking target"
        let! (result: ActionResult, mem: Memory, ctx: Context) = ActionRequest(attack target, mem)
        return Failure
        // let inflictedInjury result ctx = notImpl "detect whether the attack inflicted injury--more than just a simple success/failure check"
        // if result = Success && inflictedInjury result ctx then
        //     // keep attacking
        //     return! kill() // kill ctx // how to keep the state? Is recursion even the right way to continue here?
        // else return Failure // todo: try attacking a different target first
    }

let kill2 =
    fun (a, m, c) ->
        let bind(x,y) = notImpl()
        bind(Finished Success, fun target ->
            match target with
            | None -> Finished Success
            | Some target ->
                let mem = notImpl "attacking target"
                let q = ReturnAction(attack target, mem)
                let bhv: Behavior<_,_,_> = (fun (result, mem, ctx) ->
                    let inflictedInjury result ctx = notImpl "detect whether the attack inflicted injury--more than just a simple success/failure check"
                    if result = Success && inflictedInjury result ctx then
                        // keep attacking
                        trivial()(result, mem, ctx) // kill ctx // how to keep the state? Is recursion even the right way to continue here?
                    else Finished Failure // todo: try attacking a different target first
                    )
                bind(q, bhv)


(*
Behaviors should be
Context-aware
Composable
Have externalized memory

Consider a behavior like the following:

TRY ATTACK [X = nearest target]
    Move to X
    If DoAction Attack X succeeds but does no damage then
        Add X to TooTough list
        if can choose a new target Y
            TRY ATTACK Y
        else FAIL

COWARDLY bhv flee
    If DoBehavior bhv succeeds then keep doing it
    else DoBehavior flee

COWARDLY (TRY ATTACK bob)

We need to do several things at several points here:
Pass Bob as the initial target to TRY ATTACK (memory?)
Notice if the attack succeeds or fails (actionResult)
Evaluate whether Bob took damage (actionResult?)
Remember if we're fleeing or TRY ATTACKing (memory)

arg = Bob, no result, no memory, ctx ==> bhv ==> DoAction Attack Bob, memory[attacking Bob]
hit Bob for 0 damage, memory[attacking bob], ctx ==> bhv ==> DoAction Attack Joe, memory[Joe]
hit Joe for 0 damage, memory[attacking Joe], ctx[Sue is dead now] ==> bhv ==> flee(), memory[flee]

*)
