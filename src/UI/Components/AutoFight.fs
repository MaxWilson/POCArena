module UI.Components.AutoFight
open Domain
open Domain.CombatRules

type Page =
    | Home
    | Editing of name:string
type Opposition =
    | Calibrate of string option * int option * int option * DefeatCriteria
    | Specific of (int * string) list
type FightSetup = {
    sideA: (int * string) list
    sideB: Opposition
    }
type 't Awaitable =
    | NotStarted
    | InProgress
    | Completed of 't
type Model = {
    page: Page
    fightSetup: FightSetup
    database : MonsterDatabase
    execution: Awaitable<FightSetup * FightResult>
    }
type Side = SideA | SideB
type Msg =
    | ChangeFight of (FightSetup -> FightSetup)
    | Clear of Side
    | Upsert of Creature
    | SetPage of Page
    | Fight of (FightSetup * FightResult) Awaitable

open Fable.Core
open Feliz
open Elmish
open Fable.Core.JsInterop
open Domain
open Domain.CombatRules
open Domain.Random
open Domain.Random.Parser
open Common.UI
open Feliz.UseListener

[<Emit("$0.scrollIntoView({block: 'nearest', inline: 'nearest'})")>]
let scrollIntoView (element: Browser.Types.Node) = jsNative
[<Emit("$0.scrollIntoView({block: 'start', inline: 'nearest'})")>]
let scrollSectionIntoView (element: Browser.Types.Node) = jsNative

let update msg model =
    match msg with
    | ChangeFight f -> { model with fightSetup = f model.fightSetup }
    | SetPage page -> { model with page = page }
    | Clear side ->
        let clearSide = function
            | SideA -> { model.fightSetup with sideA = [] }
            | SideB -> { model.fightSetup with sideB = Calibrate(None, None, None, TPK) }
        { model with fightSetup = clearSide side }
    | Upsert creature ->
        if creature.name |> String.isntWhitespace then
            let db = MonsterDatabase.add creature model.database
            UI.LocalStorage.Catalog.write db.catalog
            { model with database = db }
        else model
    | Fight v -> { model with execution = v }

let init () =
    let dev = false
    let updateWithDefaults catalog =
        if dev then
            // during development, we want to be able to overwrite user defaults so they get e.g. Berserk minotaurs when we add a Berserk field
            let mutable output = catalog
            let defaults = Domain.Defaults.database()
            for k in defaults.Keys do
                output <- output |> Map.add k defaults[k]
            output
        else catalog
    let db =
        { catalog = UI.LocalStorage.Catalog.read() |> updateWithDefaults }
    let fight = {
        sideA = [3, "Peshkali"; 1, "Slugbeast"]
        sideB = Calibrate(Some "Orc", None, None, TPK)
        }
    { page = Home; fightSetup = fight; database = db; execution = NotStarted }

let beginFights (model: Model) dispatch =
    if model.execution = InProgress then
        ()
    else
        let g = System.Guid.NewGuid()
        Fight InProgress |> dispatch
        async {
            do! Async.Sleep 100 // force async to yield long enough for the busy animation to show up--I'm not sure why but it doesn't happen sometimes otherwise
            match model.fightSetup.sideB with
            | _ when (model.fightSetup.sideA |> List.sumBy fst) = 0 ->
                Fight NotStarted |> dispatch
                informUserOfError "You have to pick monsters first"
            | Calibrate(None, _, _, _) ->
                Fight NotStarted |> dispatch
                informUserOfError "You have to pick monsters first"
            | Calibrate(Some name, min, max, defeatCriteria) ->
                let min = (defaultArg min 50 |> float) / 100.
                let max = (defaultArg max 90 |> float) / 100.
                match! calibrate model.database.catalog
                        model.fightSetup.sideA
                        (name, min, max, defeatCriteria) with
                | minQuantity, maxQuantity, Some sampleMaxFight ->
                    Completed(model.fightSetup, CalibratedResult(minQuantity, maxQuantity, sampleMaxFight)) |> Fight |> dispatch
                | v ->
                    Fight NotStarted |> dispatch
                    informUserOfError "Failed to find a number of monsters that would satisfy those constraints. Try a wider range like 20% to 100%"
            | Specific(sideB) ->
                let! fightResult = specificFight model.database.catalog model.fightSetup.sideA sideB
                (model.fightSetup, SpecificResult fightResult)
                    |> Completed
                    |> Fight
                    |> dispatch
            } |> Async.StartImmediate |> ignore
