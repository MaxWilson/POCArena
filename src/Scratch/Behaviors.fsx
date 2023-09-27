#I ".."
#load "Core/Common.fs"

let enemies = ["Bob", 10; "Joe", 14; "Sue", 6] |> Map.ofList

type ActionResult = Success | Failure
type ExecutionResult<'actionOut, 'memory, 'ctx> = Finished of ActionResult | Yield | DoAction of 'actionOut * 'memory * Behavior<'actionOut, 'memory, 'ctx>
and Behavior<'actionOut, 'memory, 'ctx> = ActionResult * 'memory * 'ctx -> ExecutionResult<'actionOut, 'memory, 'ctx>
type ActionRequest<'actionOut, 'memory> = ActionRequest of 'actionOut * 'memory
type QueryRequest<'memory, 'ctx, 'result> = QueryRequest of ('memory * 'ctx -> 'result)
let bind (lhs: Behavior<'action, 'memory, 'context>) (rhs: Behavior<'action,_,'context>) : Behavior<'action, _, 'context> =
    fun (s, m, c) ->
        match lhs(s, m, c) with
        | Finished a -> rhs(a,m,c)
        | Yield -> Yield
        | DoAction(a, m', continuation) -> DoAction(a, m', notImpl())

type BehaviorBuilder() =
    member this.Return (x: ActionResult) = Finished x
    member this.ReturnFrom (x: Behavior<_,_,_>) = x
    member this.Bind(b, f) = bind b f
    member this.Bind(ActionRequest(action, mem), bhv: (ActionResult * 'memory * 'context -> ExecutionResult<_,_,_>)) =
        DoAction(action, mem, bhv)
    member this.Bind(b: QueryRequest<_,_,'result>, f: 'result -> Behavior<_,_,_>) =
        fun(a, m, c) ->
            let (QueryRequest qf) = b
            let r = qf(m,c)
            (f r)(a,m,c)
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
let almosttrivial(): Behavior<_,_,_> = behavior {
    let! (result: ActionResult, mem: Memory, ctx: Context) = ActionRequest(attack "Bob", notImpl())
    return Failure
    }

let quasitrivial = behavior {
    let! (result: ActionResult, mem: Memory, ctx: Context) = ActionRequest(attack "Bob", notImpl())
    return Failure
    }

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
                let q = ActionRequest(attack target, mem)
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