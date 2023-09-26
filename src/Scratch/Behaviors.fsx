#I ".."
#load "Core/Common.fs"

let enemies = ["Bob", 10; "Joe", 14; "Sue", 6] |> Map.ofList

type ActionResult = Success | Failure
type ExecutionResult<'action, 'memory> = Finished of ActionResult | Yield | DoAction of 'action * 'memory
type Behavior<'action, 'memory, 'ctx> = ActionResult * 'memory * 'ctx -> ExecutionResult<'action, 'memory>
type BehaviorBuilder() =
    member this.Return x = fun _ -> Finished x
    member this.Bind(b, f) = fun (s, m) -> match b(s, m) with
                                           | Some(a, m') -> f(a)(s, m')
                                           | None -> None
    member this.Delay(f) = f()
    member this.Zero() = fun _ -> None
    member this.Run(b, s, m) = b(s, m)
let behavior = BehaviorBuilder()

type SimpleAction = Attack of name: string | MoveTo of name: string
let attack (name: string) = Attack name
type AttackMemory = {
    target: string option
    }
    with static member fresh = { target = None }
type Ctx = {
    enemyHP: Map<string, int>
    }
let rec kill (ctx: Ctx) = behavior {
    let! target = fun (_, mem: AttackMemory, ctx: Ctx) -> mem.target |> Option.defaultWith (fun () -> ctx.enemyHP |> Map.tryPick (fun name hp -> if hp > 0 then Some name else None))
    match target with
    | None -> return Finished Success
    | Some target ->
        let mem = notImpl "attacking target"
        let! (result, mem, ctx) = DoAction(attack target, mem)
        let inflictedInjury result ctx = notImpl "detect whether the attack inflicted injury--more than just a simple success/failure check"
        if result = Success && inflictedInjury result ctx then
            // keep attacking
            return! kill ctx // how to keep the state? Is recursion even the right way to continue here?
        else return Finished Failure // todo: try attacking a different target first
    }

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