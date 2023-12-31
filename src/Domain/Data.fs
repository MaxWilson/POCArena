namespace Domain

[<AutoOpen>]
module Data =
    open Domain.Random

    [<Measure>] type yards
    type 't prop = 't option
    type DamageType = Cutting | Impaling | Crushing | Piercing | Burning | Other
        with
        member this.abbreviation =
            match this with
            | Cutting -> "cut"
            | Impaling -> "imp"
            | Crushing -> "cr"
            | Piercing -> "pi"
            | Burning -> "burn"
            | Other -> shouldntHappen()
    type DamageSpec = Explicit of RollSpec | Swing of int | Thrust of int
        with
        override this.ToString() =
            match this with
            | Explicit r -> toString r
            | Swing 0 -> "sw"
            | Thrust 0 -> "thr"
            | Swing v -> $"sw%+d{v}"
            | Thrust v -> $"thr%+d{v}"

    let ticksToDice ticks bonusOrPenalty =
        // every 4 dice is +1d.
        if ticks % 4 = 3 then
            RollSpec.create((ticks + 1)/4, 6, -1 + bonusOrPenalty)
        else
            RollSpec.create(ticks / 4, 6, (ticks % 4) + bonusOrPenalty)
    // Swing is (ST-6)/4 dice up to 26 (5d), then (ST+14)/8d dice up to 50 (8d), then (ST+30)/10 dice.
    // Thrust is (ST-6)/8 dice (rounded up to the next 0.25 dice) up to 70 (8d), then (ST+10)/10.
    let swingDamage st bonusOrPenalty =
        if st < 9 then RollSpec.create(1,6, bonusOrPenalty + (st-12)/2)
        elif st < 26 then ticksToDice (st-6) bonusOrPenalty
        elif st <= 50 then ticksToDice ((st+14) / 2) bonusOrPenalty
        else ticksToDice ((st+30)*4/10) bonusOrPenalty
    let thrustDamage st bonusOrPenalty =
        if st < 13 then RollSpec.create(1,6, bonusOrPenalty + (st-14)/2)
        elif st <= 70 then ticksToDice ((st-6+1) / 2) bonusOrPenalty
        else ticksToDice ((st+10+1)*4/10) bonusOrPenalty
    type InjuryTolerance = Unliving | Homogeneous | Diffuse
    type SelfControlLevel = Mild | Moderate | Serious | Severe | Always
        with
        static member toDescription =
            function
            | Mild -> "Mild (15)"
            | Moderate -> "Moderate (12)"
            | Serious -> "Serious (9)"
            | Severe -> "Severe (6)"
            | Always -> "Always"
    type Creature = {
        name: string
        pluralName: string prop
        ST: int prop
        DX: int prop
        IQ: int prop
        HT: int prop
        HP: int prop
        DR: int prop
        Dodge: int prop
        Parry: int prop
        FencingParry: bool
        Block: int prop
        ExtraAttack: int prop
        ExtraParry: int prop
        Speed: float prop
        WeaponMaster: bool
        WeaponSkill: int prop
        Damage: DamageSpec prop
        DamageType: DamageType prop
        FollowupDamage: RollSpec prop
        FollowupDamageType: DamageType prop
        UnnaturallyFragile: bool
        Berserk: SelfControlLevel prop
        HighPainThreshold: bool
        SupernaturalDurability: bool
        InjuryTolerance: InjuryTolerance prop
        AlteredTimeRate: int prop
        UseRapidStrike: bool
        CannotBeParried: bool
        }
        with
        static member create (name: string, ?pluralName) =
            { name = name
              pluralName = pluralName
              ST = None
              DX = None
              IQ = None
              HT = None
              HP = None
              Speed = None
              WeaponMaster = false
              WeaponSkill = None
              Damage = None
              DamageType = None
              FollowupDamage = None
              FollowupDamageType = None
              DR = None
              Dodge = None
              Parry = None
              FencingParry = false
              Block = None
              ExtraAttack = None
              ExtraParry = None
              UnnaturallyFragile = false
              Berserk = None
              HighPainThreshold = false
              SupernaturalDurability = false
              InjuryTolerance = None
              AlteredTimeRate = None
              UseRapidStrike = false
              CannotBeParried = false
              }
        // "_" means "defaulted" in the sense that it's the value that will be used if the property is not set.
        member this.PluralName_ = defaultArg this.pluralName (this.name + "s")
        member this.Quantify (n: int) = if n = 1 then this.name else $"{n} {this.PluralName_}"
        member this.ST_ = defaultArg this.ST 10
        member this.DX_ = defaultArg this.DX 10
        member this.IQ_ = defaultArg this.IQ 10
        member this.HT_ = defaultArg this.HT 10
        member this.HP_ = defaultArg this.HP this.ST_
        member this.Speed_ = defaultArg this.Speed ((this.DX_ + this.HT_ |> float) / 4.)
        member this.DR_ = defaultArg this.DR 0
        member this.Dodge_ = defaultArg this.Dodge ((this.Speed_ |> int) + 3)
        // notice: Parry and Block do not exist by default
        member this.ExtraAttack_ = defaultArg this.ExtraAttack 0
        member this.ExtraParry_ = defaultArg this.ExtraParry 0
        member this.WeaponSkill_ = defaultArg this.WeaponSkill 10
        member this.Damage_ =
            let addWeaponMasterDamage = function
                | RollSpec(n, d, rest) as roll when this.WeaponMaster ->
                    RollSpec.create(n*2) + roll
                | otherwise -> otherwise
            match defaultArg this.Damage (Thrust 0) with
            | Explicit roll -> roll
            | Swing bonusOrPenalty -> swingDamage this.ST_ bonusOrPenalty |> addWeaponMasterDamage
            | Thrust bonusOrPenalty -> thrustDamage this.ST_ bonusOrPenalty |> addWeaponMasterDamage
        member this.AlteredTimeRate_ = defaultArg this.AlteredTimeRate 0
        override this.ToString() =
            let names =
                match this.pluralName with
                | _ when this.name |> System.String.IsNullOrWhiteSpace -> ""
                | Some s when String.isntWhitespace s -> $"{this.name} [{s}]:"
                | _ -> $"{this.name}:"
            let labeled label = function
                | Some v -> $"{label} {v}"
                | _ -> ""
            let nolabel = function Some v -> toString v | _ -> ""
            let show label = function
                | true -> label
                | false -> ""
            let leveledOverOne label = function
                | Some 1 -> label
                | Some n -> $"{label} {n}"
                | _ -> ""
            let selfControl label = function
                | Some level -> $"{label} {toString level}" // NOT toDescription
                | None -> ""
            [   names
                labeled "ST" this.ST
                labeled "DX" this.DX
                labeled "IQ" this.IQ
                labeled "HT" this.HT
                labeled "HP" this.HP
                labeled "Speed" this.Speed
                labeled "Dodge" this.Dodge
                match this.Parry with
                    | Some p when this.FencingParry -> $"Parry {p}F"
                    | Some p -> $"Parry {p}"
                    | None -> ""
                labeled "Block" this.Block
                labeled "DR" this.DR
                labeled "Skill" this.WeaponSkill
                nolabel this.Damage
                match this.DamageType with
                    | None | Some Other -> ""
                    | Some dt -> dt.abbreviation
                labeled "+ followup" this.FollowupDamage
                match this.FollowupDamageType with
                    | None | Some Other -> ""
                    | Some dt -> if this.FollowupDamage.IsSome then dt.abbreviation else ""
                //Berserk = None
                //HighPainThreshold = false
                //SupernaturalDurability = false
                //InjuryTolerance = None
                //AlteredTimeRate = None
                //UseRapidStrike = false

                leveledOverOne "Extra Attack" this.ExtraAttack
                leveledOverOne "Extra Parry" this.ExtraParry
                leveledOverOne "Altered Time Rate" this.AlteredTimeRate
                show "Weapon Master" this.WeaponMaster
                show "Unnatural" this.UnnaturallyFragile
                show "Supernatural Durability" this.SupernaturalDurability
                show "High Pain Threshold" this.HighPainThreshold
                show "Rapid Strike" this.UseRapidStrike
                show "Cannot Be Parried" this.CannotBeParried
                selfControl "Berserk" this.Berserk
                match this.InjuryTolerance with None -> "" | Some it -> it.ToString()


                ] |> List.filter String.isntWhitespace |> String.concat " "

    type MonsterDatabase = {
        catalog: Map<string, Creature>
        }
        with
        static member fresh = { catalog = Map.empty }
        static member add (monster: Creature) (db: MonsterDatabase) =
            { catalog = Map.add monster.name monster db.catalog }

    type Status = OK | Stunned | Prone | Unconscious | Dead | Berserk
    type CombatantId = int * string
    type HitSide = Left | Right
    type HitLocation = Head | Torso | Arm of HitSide | Leg of HitSide | Hand of HitSide | Foot of HitSide | Vitals | Neck | Groin | Eye of HitSide | Skull
    type Destination = Person of CombatantId | Place of int * int
    type AttackDetails = {
        // The difference between AttackDetails vs. Combatant is that AttackDetails is an "untrusted"
        // request from the user--the user can request illegal levels of deceptive for example, or
        // multiple Rapid Strikes. It's up to the GMing logic to verify rules compliance as part of
        // action resolution.
        target: CombatantId
        rapidStrike: bool
        location: HitLocation option
        deceptiveLevels: int
        }
        with static member create(target) = { target = target; rapidStrike = false; location = None; deceptiveLevels = 0 }
    type Action =
        | Attack of AttackDetails
        | Move of Destination
        | Yield // end turn, wait for things to change
    type Coords = float<yards> * float<yards>
    type Distance = float<yards>
    type Combatant = {
        personalName: string
        number: int
        team: int
        stats: Creature
        injuryTaken: int
        shockPenalty: int
        statusMods: Status list
        retreatUsed: CombatantId option
        blockUsed: bool
        parriesUsed: int
        attacksUsed: int
        rapidStrikeUsed: bool
        coords: Coords
        }
        with
        member this.CurrentHP_ = this.stats.HP_ - this.injuryTaken
        member this.Id : CombatantId = this.team, this.personalName
        static member fresh (team, name, number, coords, stats: Creature) =
            {   team = team
                personalName = name
                number = number
                stats = stats
                injuryTaken = 0
                shockPenalty = 0
                statusMods = if stats.Berserk = Some Always then [Berserk] else []
                retreatUsed = None
                blockUsed = false
                parriesUsed = 0
                attacksUsed = 0
                rapidStrikeUsed = false
                coords = coords
                }
        member this.is (status: Status) = this.statusMods |> List.exists ((=) status)
        member this.isAny (statuses: Status list) = this.statusMods |> List.includes statuses
        member this.isnt (status: Status) = this.is status |> not
        member this.isnt (statuses: Status list) = this.isAny statuses |> not
        static member newTurn (combatant: Combatant) =
            { combatant
                with
                retreatUsed = None
                blockUsed = false
                parriesUsed = 0
                attacksUsed = 0
                rapidStrikeUsed = false
                shockPenalty = 0
                }

    type Combat = {
        combatants: Map<CombatantId, Combatant>
        }
    type Ids =
        { attacker: CombatantId; target: CombatantId }
        with
        member this.Attacker_ = snd this.attacker
        member this.AttackerTeam_ = fst this.attacker
        member this.Target_ = snd this.target
        member this.TargetTeam_ = fst this.target
    type DefenseType = Parry | Block | Dodge
    type DefenseDetails = { defense: DefenseType; retreatFrom: CombatantId option }
        with member this.targetRetreated = this.retreatFrom.IsSome

    [<AutoOpen>]
    module CombatEvents =
        type Event =
            | Hit of Ids * DefenseDetails option * injury:int * Status list * string
            | SuccessfulDefense of Ids * DefenseDetails * string
            | Miss of Ids * string
            | FallUnconscious of CombatantId * string
            | Unstun of CombatantId * string
            | StandUp of CombatantId * string
            | Info of CombatantId * msg: string * rollInfo: string
            | NewRound of int
        let update msg model =
            let updateCombatant id (f: Combatant -> Combatant) model =
                {   combatants = model.combatants |> Map.change id (function | Some c -> Some (f c) | None -> None)
                    }
            let consumeDefense (id: CombatantId) (defense: DefenseDetails option) =
                updateCombatant id (fun c ->
                    match defense with
                    | Some defense ->
                        { c with
                            retreatUsed = c.retreatUsed |> Option.orElse defense.retreatFrom
                            blockUsed = c.blockUsed || defense.defense = Block
                            parriesUsed = c.parriesUsed + (if defense.defense = Parry then 1 else 0)
                            }
                    | None -> c)
            let newTurn (id: CombatantId) =
                updateCombatant id Combatant.newTurn
            let takeDamage (id: CombatantId) amount conditions =
                updateCombatant id (fun c ->
                    let goingBerserk = conditions |> List.contains Berserk
                    // if going berserk, make sure to remove Stunned from conditions
                    let mods' = (c.statusMods @ conditions) |> List.distinct
                    { c with
                        injuryTaken = c.injuryTaken + amount
                        shockPenalty =
                            if c.stats.SupernaturalDurability || c.stats.HighPainThreshold || (mods' |> List.contains Berserk) then 0
                            else (c.shockPenalty - (amount / (max 1 (c.stats.HP_ / 10)))) |> max -4
                        statusMods = if goingBerserk then mods' |> List.filter ((<>) Stunned) else mods'
                        })
            match msg with
            | Hit (ids, defense, injury, statusImpact, rollDetails) ->
                model |> newTurn ids.attacker
                    |> consumeDefense ids.target defense
                    |> takeDamage ids.target injury statusImpact
            | SuccessfulDefense(ids, defense, rollDetails) ->
                model |> newTurn ids.attacker
                    |> consumeDefense ids.target (Some defense)
            | Miss (ids, rollDetails) ->
                model |> newTurn ids.attacker
            | FallUnconscious(id, rollDetails) ->
                model |> newTurn id |> takeDamage id 0 [Unconscious]
            | Unstun(id, rollDetails) ->
                model |> newTurn id
                    |> updateCombatant id (fun c ->
                            { c with statusMods = c.statusMods |> List.filter ((<>) Stunned) })
            | StandUp(id, rollDetails) ->
                model |> newTurn id
                    |> updateCombatant id (fun c ->
                            { c with statusMods = c.statusMods |> List.filter ((<>) Prone) })
            | Info (id, _, _) -> model |> newTurn id
            | NewRound _ -> model
    type CombatLog = (Event option * Combat) list

    type DefeatCriteria =
        | TPK
        | OneCasualty
        | HalfCasualties
    type FightResult =
        | CalibratedResult of lower:int option * upper:int option * sample:CombatLog
        | SpecificResult of CombatLog * {| victors: int list |}
    type Outcome = CritSuccess of int | Success of int | CritFail of int | Fail of int
    type 'members GroupSetup = {
        members: 'members
        center: Coords
        radius: Distance option
        }
    type GroupSetup = ((int * string) list) GroupSetup
    type TeamSetup = GroupSetup list
    type Opposition =
    | Calibrate of GroupSetup<string option * int option * int option * DefeatCriteria>
    | Specific of GroupSetup list
        with
        static member calibrated members setPosition = Calibrate (members |> setPosition)
    type FightSetup = {
        sideA: TeamSetup
        sideB: Opposition
        }
        with static member fresh setPosition = { sideA = []; sideB = Opposition.calibrated (None, None, None, TPK) setPosition }


#nowarn "40" // we're not planning on doing any unsafe things during initialization, like evaluating the functions that rely on the object we're busy constructing
module Parser =
    open Packrat
    open Random
    open Random.Parser
    let (|DamageType|_|) = pack <| function
        | OWSStr "crushing" rest
        | OWSStr "cr" rest -> Some(DamageType.Crushing, rest)
        | OWSStr "cutting" rest
        | OWSStr "cut" rest -> Some(DamageType.Cutting, rest)
        | OWSStr "impaling" rest
        | OWSStr "imp" rest -> Some(DamageType.Impaling, rest)
        | OWSStr "piercing" rest
        | OWSStr "pierce" rest
        | OWSStr "pi" rest -> Some(DamageType.Piercing, rest)
        | OWSStr "burning" rest
        | OWSStr "burn" rest -> Some(DamageType.Burning, rest)
        | _ -> None
    let (|OptionalDamageType|_|) = pack <| function
        | DamageType(dt, rest) -> Some(Some dt, rest)
        | rest -> Some(None, rest)
    let (|OptionalIntMod|_|) = pack <| function
        | OWS (IntModifier(bonusOrPenalty, rest)) -> Some(bonusOrPenalty, rest)
        | rest -> Some(0, rest)
    let (|SelfControlLevel|_|) = pack <| function
        | OWSStr "15" rest -> Some(Mild, rest)
        | OWSStr "Mild" rest -> Some(Mild, rest)
        | OWSStr "12" rest -> Some(Moderate, rest)
        | OWSStr "Moderate" rest -> Some(Moderate, rest)
        | OWSStr "9" rest -> Some(Serious, rest)
        | OWSStr "Serious" rest -> Some(Serious, rest)
        | OWSStr "6" rest -> Some(Severe, rest)
        | OWSStr "Severe" rest -> Some(Severe, rest)
        | OWSStr "Auto" rest -> Some(Always, rest)
        | OWSStr "Always" rest -> Some(Always, rest)
        | rest -> None
    let (|OptionalInt|_|) = pack <| function
        | OWS (Int(n, rest)) -> Some(Some n, rest)
        | rest -> Some(None, rest)
    let (|DamageOverall|_|) = pack <| function
        | Roll(roll, OptionalDamageType(dt, rest)) -> Some((Explicit roll, dt), rest)
        | OWSStr "swing" (OptionalIntMod(bonusOrPenalty, OptionalDamageType(dt, rest))) -> Some((Swing bonusOrPenalty, dt), rest)
        | OWSStr "sw" (OptionalIntMod(bonusOrPenalty, OptionalDamageType(dt, rest))) -> Some((Swing bonusOrPenalty, dt), rest)
        | OWSStr "thrust" (OptionalIntMod(bonusOrPenalty, OptionalDamageType(dt, rest))) -> Some((Thrust bonusOrPenalty, dt), rest)
        | OWSStr "thr" (OptionalIntMod(bonusOrPenalty, OptionalDamageType(dt, rest))) -> Some((Thrust bonusOrPenalty, dt), rest)
        | _ -> None
    let (|CreatureProperty|_|) =
        pack <| function
        // e.g. ST 17 DX 12 IQ 9 HT 11 HP 22 Speed 14 Weapon Master Skill 22 sw+2 cut
        | OWSStr "ST" (Int (v, rest)) -> Some((fun c -> { c with ST = Some v }), rest)
        | OWSStr "DX" (Int (v, rest)) -> Some((fun c -> { c with DX = Some v }), rest)
        | OWSStr "IQ" (Int (v, rest)) -> Some((fun c -> { c with IQ = Some v }), rest)
        | OWSStr "HT" (Int (v, rest)) -> Some((fun c -> { c with HT = Some v }), rest)
        | OWSStr "HP" (Int (v, rest)) -> Some((fun c -> { c with HP = Some v }), rest)
        | OWSStr "DR" (Int (v, rest)) -> Some((fun c -> { c with DR = Some v }), rest)
        | OWSStr "Speed" (Decimal (v, rest)) -> Some((fun c -> { c with Speed = Some v }), rest)
        | OWSStr "Dodge" (Int (v, rest)) -> Some((fun c -> { c with Dodge = Some v }), rest)
        | OWSStr "Parry" (Int (v, Str "F" rest)) -> Some((fun c -> { c with Parry = Some v; FencingParry = true }), rest)
        | OWSStr "Parry" (Int (v, rest)) -> Some((fun c -> { c with Parry = Some v }), rest)
        | OWSStr "Block" (Int (v, rest)) -> Some((fun c -> { c with Block = Some v }), rest)
        | OWSStr "Weapon Master" rest -> Some((fun c -> { c with WeaponMaster = true }), rest)
        | OWSStr "Skill" (Int (v, rest)) -> Some((fun c -> { c with WeaponSkill = Some v }), rest)
        | DamageOverall((damage, damageType), OWSStr "+" (OWSStr "followup" (OWS (Roll((followupDamage, OptionalDamageType(followupDamageType, rest)))))))
            -> Some((fun c -> { c with Damage = Some damage; DamageType = damageType; FollowupDamage = Some followupDamage; FollowupDamageType = followupDamageType }), rest)
        | DamageOverall((damage, damageType), rest) -> Some((fun c -> { c with Damage = Some damage; DamageType = damageType }), rest)
        | OWSStr "Rapid Strike" rest -> Some((fun c -> { c with UseRapidStrike = true }), rest)
        | OWSStr "Cannot Be Parried" rest -> Some((fun c -> { c with CannotBeParried = true }), rest)
        | OWSStr "Extra Attack" (Int (v, rest)) -> Some((fun c -> { c with ExtraAttack = Some v }), rest)
        | OWSStr "Extra Attack" rest -> Some((fun c -> { c with ExtraAttack = Some 1 }), rest)
        | OWSStr "Extra Parry" (Int (v, rest)) -> Some((fun c -> { c with ExtraParry = Some v }), rest)
        | OWSStr "Unliving" rest -> Some((fun c -> { c with InjuryTolerance = Some Unliving }), rest)
        | OWSStr "Homogeneous" rest -> Some((fun c -> { c with InjuryTolerance = Some Homogeneous }), rest)
        | OWSStr "Diffuse" rest -> Some((fun c -> { c with InjuryTolerance = Some Diffuse }), rest)
        | OWSStr "High Pain Threshold" rest -> Some((fun c -> { c with HighPainThreshold = true }), rest)
        | OWSStr "HPT" rest -> Some((fun c -> { c with HighPainThreshold = true }), rest)
        | OWSStr "Supernatural Durability" rest -> Some((fun c -> { c with SupernaturalDurability = true }), rest)
        | OWSStr "Unnatural" rest -> Some((fun c -> { c with UnnaturallyFragile = true }), rest)
        | OWSStr "Altered Time Rate" (OptionalInt (v, rest)) -> Some((fun c -> { c with AlteredTimeRate = Some (defaultArg v 1) }), rest)
        | OWSStr "ATR" (OptionalInt (v, rest)) -> Some((fun c -> { c with AlteredTimeRate = Some (defaultArg v 1) }), rest)
        | OWSStr "Berserk" (SelfControlLevel(level, rest)) -> Some((fun c -> { c with Berserk = Some level }), rest)
        | _ -> None
    let rec (|CreatureProperties|_|) = pack <| function
        | CreatureProperties(fprops, CreatureProperty(fprop, rest)) -> Some(fprops >> fprop, rest)
        | CreatureProperty(fprop, rest) ->
            Some(fprop, rest)
        | _ -> None
    let (|Creature|_|) = pack <| function
        | Words(name, OWSStr "[" (Words(plural, OWSStr "]" (OWSStr ":" (CreatureProperties(fprops, rest)))))) ->
            Some(Creature.create(name, plural) |> fprops, rest)
        | Words(name, OWSStr ":" (CreatureProperties(fprops, rest))) ->
            Some(Creature.create(name) |> fprops, rest)
        | _ -> None
    let (|Command|_|) = pack <| function
        | Str "add" (Creature(monster, rest)) -> Some((fun db -> MonsterDatabase.add monster db), rest)
        | _ -> None

module Defaults =
    open Packrat
    let database() =
        [
            let parse (input: string) =
                match ParseArgs.Init input with
                | Parser.Creature(c, OWS End) -> c
                | _ -> shouldntHappen input
            parse "Peshkali [Peshkalir]: ST 20 DX 12 HT 12 DR 4 Skill 18 sw+1 cut Extra Attack 5 Extra Parry 5 Parry 13 Dodge 10 Supernatural Durability"
            parse "Orc: ST 12 DX 11 IQ 9 HT 11 DR 2 HP 14 Dodge 7 Parry 9 Block 9 Skill 13 sw+1 cut"
            parse "Ogre: ST 20 DX 11 IQ 7 HT 13 High Pain Threshold Skill 16 3d+7 cr Parry 11 DR 3"
            parse "Slugbeast: ST 16 IQ 2 Skill 12 1d+2 Homogeneous"
            parse "Skeleton: ST 11 DX 13 IQ 8 HT 12 Skill 14 thr+3 imp DR 2 Speed 8 Parry 10 Block 10 Unliving Unnatural"
            parse "Stone Golem: ST 20 DX 11 IQ 8 HT 14 HP 30 HPT Parry 9 DR 4 Homogeneous Skill 13 sw+4 cut Unnatural"
            parse "Rock Mite: ST 12 HT 13 Speed 5.00 DR 5 Homogeneous Skill 10 1d-1 cut + followup 2d burn"
            parse "Inigo Montoya: ST 13 DX 16 IQ 11 HT 12 Speed 8.5 Dodge 12 Parry 17F DR 1 Weapon Master Skill 22 thr+2 imp Extra Attack 1 Rapid Strike"
            parse "Cave Bear: ST 23 DX 11 IQ 4 HT 13 DR 2 Parry 9 Skill 13 2d thr+1 cut Berserk 9"
            parse "Watcher: ST 12 DX 18 HT 12 Speed 10 Dodge 14 Parry 13 Skill 18 sw cut Extra Parry 3 Extra Attack 3 Altered Time Rate"
            parse "Mafia Gunman [Mafia Gunmen]: ST 10 DX 12 HT 11 Skill 12 Cannot Be Parried 2d+2 pi"
            ]
        |> List.map(fun c -> c.name, c)
        |> Map.ofList
