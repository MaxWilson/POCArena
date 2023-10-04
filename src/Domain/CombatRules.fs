module Domain.CombatRules
open Domain
open Domain.Random
open Domain.Behavior

let successTest target x =
    if x >= target + 10 then CritFail (x - target)
    elif x = 17 then if target >= 16 then Fail(x - 16) else CritFail (x - 16)
    elif x = 18 then CritFail (x - target)
    elif x > target then Fail (x - target)
    elif target >= 16 && x <= 6 then CritSuccess ((min target 6) - x)
    elif target >= 15 && x <= 5 then CritSuccess ((min target 5) - x)
    elif x <= 4 then CritSuccess ((min target 4) - x)
    else Success (target - x)

let chooseDefense (attacker: Combatant) (victim: Combatant) =
    let attackerId = attacker.Id
    let canRetreat =
        (not <| victim.isAny [Dead; Unconscious; Stunned] )
        && (
        match victim.retreatUsed with
        | Some id when id = attackerId -> true
        | None -> true
        | _ -> false)
    let (|Parry|_|) = function
        | Some parry when not attacker.stats.CannotBeParried ->
            let parry = (parry - (match victim.stats.WeaponMaster, victim.stats.FencingParry with | true, true -> 1 | true, false | false, true -> 2 | otherwise -> 4) * (victim.parriesUsed / (1 + victim.stats.ExtraParry_)))
            Some(if canRetreat then (if victim.stats.FencingParry then 3 else 1) + parry, Some attackerId else parry, None)
        | _ -> None
    let (|Block|_|) = function
        | Some block when victim.blockUsed = false ->
            Some(if canRetreat then 1 + block, Some attackerId else block, None)
        | _ -> None
    let dodge, retreat =
        let dodge = if (float victim.CurrentHP_) >= (float victim.stats.HP_ / 3.)
                    then victim.stats.Dodge_
                    else victim.stats.Dodge_ / 2
        if canRetreat then 3 + dodge, Some attackerId else dodge, None
    let target, defense =
        match victim.stats.Parry, victim.stats.Block with
        | Parry (parry, retreat), Block (block, _) when parry >= block && parry >= dodge ->
            // I guess we'll use parry in this case because we have to pick something
            parry, { defense = Parry; retreatFrom = retreat }
        | _, Block (block, retreat) when block >= dodge ->
            block, { defense = Block; retreatFrom = retreat }
        | Parry (parry, retreat), _ when parry >= dodge ->
            parry, { defense = Parry; retreatFrom = retreat }
        | _ ->
            dodge, { defense = Dodge; retreatFrom = retreat }
    let target =
        target
        + (if victim.is Stunned then -4 else 0)
        + (if victim.is Prone then -3 else 0)
    target, defense

let failedDeathcheck (attempt: int -> bool) (fullHP: int) priorHP newHP =
    if newHP <= fullHP * -1 then
        let oldBracket = priorHP / fullHP |> min 0
        let newBracket = newHP / fullHP
        let checksNeeded = oldBracket - newBracket
        let rec loop checksFinished =
            let threshold = ((-oldBracket + 1) + checksFinished) * -fullHP
            if checksFinished = checksNeeded then false // Zero or more
            elif attempt threshold = false then
                true // failed!
            else loop (checksFinished+1)
        loop 0
    else false

let fightOneRound (cqrs: CQRS.CQRS<_, Combat>) =
    // HIGH speed and DX goes first so we use the negative of those values
    for c in cqrs.State.combatants.Values |> Seq.sortBy (fun c -> -c.stats.Speed_, -c.stats.DX_, c.stats.name, c.number) |> Seq.map (fun c -> c.Id) do
        let roll3d6 = RollSpec.create(3,6)
        let mutable msg = ""
        let recordMsg txt =
            if msg = "" then msg <- txt else msg <- $"{msg}; {txt}"
        let detailedAttempt label targetNumber =
            let roll = roll3d6.roll()
            match successTest targetNumber roll with
            | CritSuccess _ as success ->
                recordMsg $"{label} critically succeeded (target {targetNumber}, rolled {roll})"
                success
            | Success _ as success ->
                recordMsg $"{label} succeeded (target {targetNumber}, rolled {roll})"
                success
            | Fail _ as fail ->
                recordMsg $"{label} failed (target {targetNumber}, rolled {roll})"
                fail
            | CritFail _ as fail ->
                recordMsg $"{label} failed (target {targetNumber}, rolled {roll})"
                fail
        let attempt label targetNumber =
            match detailedAttempt label targetNumber with
            | (CritSuccess _ | Success _) as success -> true
            | (CritFail _ | Fail _) -> false
        let checkGoesUnconscious (self: Combatant, isBerserk) incomingDamage =
            let penalty = (self.CurrentHP_ - incomingDamage) / self.stats.HP_
            attempt "Stay conscious" (self.stats.HT_ + penalty + (if isBerserk then +4 else 0)) |> not
        let mutable doneEarly = false
        let attacker = cqrs.State.combatants[c]
        if attacker.isnt [Dead; Unconscious] then
            if attacker.is Stunned then
                if attempt "Recover from stun" attacker.stats.HT_ then
                    Unstun(attacker.Id, msg)
                else
                    Info(attacker.Id, "does nothing", msg)
                |> cqrs.Execute
            elif attacker.CurrentHP_ <= 0 && (not attacker.stats.SupernaturalDurability) && checkGoesUnconscious (attacker, attacker.is Berserk) 0 then
                FallUnconscious(attacker.Id, msg)
                |> cqrs.Execute
            elif attacker.is Prone then
                StandUp(attacker.Id, msg)
                |> cqrs.Execute
            else
                for m in 1..(1 + attacker.stats.AlteredTimeRate_) do
                    let totalAttacks, rapidStrikes =
                        (if attacker.stats.UseRapidStrike then 2 + attacker.stats.ExtraAttack_, 2 else 1 + attacker.stats.ExtraAttack_, 0)
                    let totalAttacks = if attacker.is Berserk then totalAttacks + 1 else totalAttacks
                    for n in 1..totalAttacks do
                        if (not doneEarly) then
                            match attacker |> tryFindTarget cqrs.State with
                            | Some victim ->
                                let skill, defensePenalty =
                                    let rapidStrikePenalty =
                                        if n <= rapidStrikes then
                                            let penalty = if attacker.stats.WeaponMaster then -3 else -6
                                            recordMsg $"Using Rapid Strike %+d{penalty}"
                                            penalty
                                        else 0
                                    if attacker.shockPenalty <> 0 then
                                        recordMsg $"Shock penalty %+d{attacker.shockPenalty}"
                                    match (attacker.stats.WeaponSkill_ + attacker.shockPenalty + rapidStrikePenalty) with
                                    | n when n >= 18 ->
                                        let deceptive = (n - 16)/2
                                        recordMsg $"Using Deceptive Attack {-2 * deceptive}"
                                        n - deceptive * 2, deceptive
                                    | n -> n, 0
                                match detailedAttempt "Attack" skill with
                                | (Success _ | CritSuccess _) as success ->
                                    let defenseTarget, defense = chooseDefense attacker victim
                                    let defenseLabel =
                                        (match defense.defense with Parry -> "Parry" | Block -> "Block" | Dodge -> "Dodge")
                                        + (if defense.targetRetreated then " and retreat" else "")
                                    let critSuccess = match success with CritSuccess _ -> true | _ -> false
                                    if not (critSuccess || victim.is Berserk) && attempt defenseLabel (defenseTarget - defensePenalty) then
                                        SuccessfulDefense({ attacker = attacker.Id; target = victim.Id }, defense, msg)
                                    else
                                        let defense =
                                            if critSuccess then None
                                            else Some defense
                                        let damageCap damageType = max (if damageType = Some Crushing then 0 else 1)
                                        let dmg = attacker.stats.Damage_.roll() |> damageCap attacker.stats.DamageType
                                        let penetratingDmg = dmg - victim.stats.DR_ |> max 0
                                        let toInjury (penetratingDmg, damageType) =
                                            match victim.stats.InjuryTolerance, damageType with
                                            | Some Diffuse, Some (Impaling | Piercing) -> max 1 penetratingDmg
                                            | Some Diffuse, _ -> max 2 penetratingDmg
                                            | Some Homogeneous, Some Impaling -> penetratingDmg / 2
                                            | Some Homogeneous, Some Piercing -> penetratingDmg / 5
                                            | Some Unliving, Some Impaling -> penetratingDmg
                                            | Some Unliving, Some Piercing -> penetratingDmg / 3
                                            | _, Some Cutting -> (float penetratingDmg * 1.5) |> int
                                            | _, Some Impaling -> penetratingDmg * 2
                                            | _ -> penetratingDmg
                                        let injury = toInjury (penetratingDmg, attacker.stats.DamageType)
                                        // add followup damage, and log the total damage and injury
                                        let injury =
                                            match attacker.stats.FollowupDamage with
                                            | Some r when penetratingDmg > 0 ->
                                                let followup, followupType = (r.roll() |> damageCap attacker.stats.FollowupDamageType, attacker.stats.FollowupDamageType)
                                                let injury = injury + toInjury (followup, followupType)
                                                recordMsg $"Damage {attacker.stats.Damage_} + {r} ({dmg} {defaultArg attacker.stats.DamageType Other}, {followup} {defaultArg followupType Other}) - DR {victim.stats.DR_} = {injury} injury"
                                                injury
                                            | _ ->
                                                recordMsg $"Damage {attacker.stats.Damage_} ({dmg} {defaultArg attacker.stats.DamageType Other}) - DR {victim.stats.DR_} = {injury} injury"
                                                injury
                                        let mutable newConditions = []
                                        let mutable berserk = victim.is Berserk
                                        match victim.stats.Berserk with
                                        | Some berserkLevel when (float injury > float victim.stats.HP_ / 4. && victim.isnt Berserk) ->
                                            let target =
                                                match berserkLevel with Mild -> 15 | Moderate -> 12 | Serious -> 9 | Severe -> 6 | Always -> 0
                                            // we deliberately don't use attempt here because we don't want to clutter the log with self-control rolls
                                            if (roll3d6.roll() <= target = false) then
                                                recordMsg $"{victim.personalName} goes berserk"
                                                newConditions <- newConditions@[Berserk]
                                                berserk <- true
                                        | _ -> ()
                                        let hp' = victim.CurrentHP_ - injury
                                        // -5 x max HP is auto-death
                                        let autodeathThreshold = victim.stats.HP_ * (if victim.stats.UnnaturallyFragile then -1 else -5)
                                        if hp' <= autodeathThreshold then
                                            recordMsg $"Auto-death occurs at {autodeathThreshold} HP"
                                            newConditions <- [Dead]
                                        // check for death if crossing a HP threshold, -1 x max HP or below
                                        elif failedDeathcheck (fun threshold -> attempt $"Deathcheck at {threshold} HP" (victim.stats.HT_ + if berserk then +4 else 0))
                                                victim.stats.HP_ victim.CurrentHP_ hp' then
                                            newConditions <- [Dead]
                                        // check for unconsciousness on dropping to zero HP
                                        elif victim.CurrentHP_ > 0 && hp' <= 0 && (not victim.stats.SupernaturalDurability) && checkGoesUnconscious (victim, berserk) injury then
                                            newConditions <- [Unconscious]
                                        elif injury > (victim.stats.HP_ + 1) / 2 && not (victim.stats.SupernaturalDurability || berserk)
                                                && (attempt "Knockdown check" (victim.stats.HT_ +
                                                    if victim.stats.HighPainThreshold then +3 else 0) |> not) then
                                            newConditions <- [Stunned; Prone]
                                        Hit({ attacker = attacker.Id; target = victim.Id }, defense, injury, newConditions, msg)
                                | (Fail _ | CritFail _) ->
                                    Miss({ attacker = attacker.Id; target = victim.Id }, msg)
                            | None ->
                                doneEarly <- true
                                Info(attacker.Id, "can't find a victim", msg)
                            |> cqrs.Execute
                            msg <- "" // if multiple attacks process we don't want to redisplay the same text


let fight (cqrs: CQRS.CQRS<_,Combat>) =
    let rec loop counter =
        let survivingTeams =
            let everyone = cqrs.State.combatants.Values |> List.ofSeq
            everyone |> List.choose (fun c -> if c.isnt [Dead; Unconscious] then Some c.team else None)
                     |> List.distinct
        if survivingTeams.Length < 2 || counter > 100 then
            // it's possible to have a tie or a mutual kill
            {| victors = survivingTeams |}
        else
            if counter > 1 then
                cqrs.Execute (NewRound(counter))
            fightOneRound cqrs
            loop (counter + 1)
    loop 1

let radius_ (group:GroupSetup) =
    match group.radius with
    | Some r -> r
    | None ->
        // we want it full but not THAT full
        let memberCount = group.members |> List.sumBy fst
        1.0<yards> * (sqrt (float memberCount))

let toCombatants (db: Map<string, Creature>) teamNumber positioner =
    // we want numbers to ascend smoothly on a side, so that we can use numbers to prioritize targets in the same order they were in fightsetup
    let mutable counter = 0
    let mutable perMonsterCounter = Map.empty
    fun (team: TeamSetup) ->
        [   for group in team do
                for quantity, name in group.members do
                    for i in 1..quantity do
                        let stats = db[name]
                        let prev = defaultArg (perMonsterCounter.TryFind name) 0 // if there are multiple groups of e.g. 1 orc and 1 orc, the second group should start at Orc 2 not "Orc"
                        Combatant.fresh(teamNumber, (if prev + quantity > 1 then $"{name} {prev + i}" else name), counter + i, positioner (group.center, radius_ group, stats), stats)
                    counter <- counter + quantity
                    perMonsterCounter <- perMonsterCounter |> Map.add name (defaultArg (perMonsterCounter.TryFind name) 0 + quantity)
            ]

let makeGroupPositioner ()  = // a groupPositioner is stateful so it can keep track of which spaces are still empty
    let mutable occupiedCells = Set.empty
    fun (center: Coords, radius: float<yards>, stats) ->
        let gen() =
            let angleRadians = random.NextDouble() * 2. * System.Math.PI
            let radius = random.NextDouble() * radius
            let x = cos angleRadians * radius + fst center |> Ops.round
            let y = sin angleRadians * radius + snd center |> Ops.round
            x, y
        let rec loop failureCount radius candidate =
            let x,y = candidate
            if occupiedCells.Contains (int x, int y) then
                if failureCount > 10 then loop 0 (radius + 1.<yards>) (gen()) // maybe it's full; widen the radius so we don't get stuck in an infinite loop
                else loop (failureCount+1) radius (gen())
            else
                occupiedCells <- occupiedCells.Add (int x, int y)
                candidate
        loop 0 radius (gen())

let createCombat (db: Map<string, Creature>) (team1: TeamSetup) team2 =
    let positioner = makeGroupPositioner() // stateful!
    { combatants =
        (team1 |> (toCombatants db 1 positioner)) @ (team2 |> (toCombatants db 2 positioner))
        |> Seq.map(fun c -> c.Id, c)
        |> Map.ofSeq
        }
let specificFight db team1 team2 = async {
    let cqrs = CQRS.CQRS.Create((createCombat db team1 team2), update)
    let victors = fight cqrs
    return cqrs.LogWithMessages(), victors
    }
module Team =
    let randomInitialPosition members : _ GroupSetup =
        let yards n = float n * 1.<yards>
        {   members = members
            // we'll use the middle 30 x 30 as the default center instead of the whole 40 x 40 area
            center = (5 + random.Next 29 |> yards, 5 + random.Next 29 |> yards)
            radius = None
            }

    let fresh (monsters: (int * string) list): TeamSetup = monsters |> List.map (fun m -> randomInitialPosition [m])
    let freshCalibrated() = Opposition.calibrated (None, None, None, TPK) randomInitialPosition

let calibrate db (team1: TeamSetup) (center: Coords, radius: Distance, enemyType, minbound, maxbound, defeatCriteria) = async {
    let runForN n = async {
        do! Async.Sleep 0 // yield the JS runtime  in case UI updates need to be processed
        let combat = createCombat db team1 (Team.fresh [n, enemyType ]) // instantiate. TODO: instantiate at specific positions, as soon as monsters have positions.
        let cqrs = CQRS.CQRS.Create(combat, update)
        return cqrs, fight cqrs
        }
    let mutable results: Map<_,int*CombatLog> = Map.empty
    let get n = async {
        if results.ContainsKey n then return results[n]
        else
            let! runs =
                [
                    for run in 1..10 do
                        runForN n
                    ]
                |> Async.Parallel
            let sampleLog: CombatLog = (runs |> Array.last |> fst).LogWithMessages()
            let victoryMetric : CQRS.CQRS<_, Combat> * {| victors: int list |} -> int =
                match defeatCriteria with
                | TPK -> function (_, v) when v.victors = [1] -> 1 | otherwise -> 0
                | OneCasualty ->
                    fun (cqrs, v) ->
                        // in this case, TeamA is very casualty-averse. Defeat is taking even one casualty (dead or unconscious).
                        if cqrs.State.combatants.Values |> Seq.exists (fun c ->
                            c.team = 1 && c.isAny[Dead; Unconscious]) then
                            0
                        else 1
                | HalfCasualties ->
                    fun (cqrs, v) ->
                        // in this case, TeamA is somewhat casualty-averse. Defeat is a pyrrhic victory where at least half the team dies.
                        let friendlies = cqrs.State.combatants.Values |> Seq.filter (fun c -> c.team = 1)
                        let deadFriendlies = friendlies |> Seq.filter (fun c -> c.isAny[Dead; Unconscious])
                        if deadFriendlies |> Seq.length >= ((friendlies |> Seq.length) / 2) then
                            0
                        else 1

            let victories = runs |> Array.sumBy victoryMetric
            results <- results |> Map.add n (victories, sampleLog)
            return results[n]
        }
    // crude and naive model: search from 1 to 100, but quit early when we fall to 0% victory
    let! upToOneHundred =
        let rec loop n = async {
            let! (victories, log) as result = get n
            if victories > 0 && n <= 100 then
                let! looped = loop (n+1)
                return (n, result)::looped
            else return []
            }
        loop 1
    let inbounds (n, result) =
        betweenInclusive (minbound * 10. |> int) (maxbound * 10. |> int) (result |> fst)

    match upToOneHundred |> List.filter inbounds with
    | [] ->
        return None, None, None
    | inbounds ->
        let min, _ = inbounds |> List.minBy fst
        let max, (_, sampleFight) = inbounds |> List.maxBy fst
        return Some min, Some max, Some sampleFight
    }