module UI.Components.ArenaView

open System
open UI.Konva
open Feliz
open Feliz.UseElmish
open Elmish
open Domain.Data
open UI.Components.Arena

[<AutoOpen>]
module private Impl =
    type RenderHelper(pixelWidth, pixelHeight) =
        let _scaleX, _scaleY = (float pixelWidth / 40.<yards>), (float pixelHeight / 40.<yards>)
        // hmmm, I guess we want to use the same scale for both X and Y don't we? Take the minimum and just let the other space go unused.
        let _scaleX, _scaleY = let m = min _scaleX _scaleY in m, m
        member _.scaleX (x: float<yards>) = x * _scaleX
        member _.unscaleX (x: float) = x / _scaleX
        member _.scaleY (y: float<yards>) = y * _scaleY
        member _.unscaleY (y: float) = y / _scaleY
        member _.rect name (x:float<yards>,y:float<yards>) props =
            Rect.create ([
                Rect.x (x * _scaleX)
                Rect.y (y * _scaleY)
                Rect.key name
                ]
                @props)
        member _.circle name (x:float<yards>,y:float<yards>) props =
            Circle.create ([
                Circle.x (x * _scaleX)
                Circle.y (y * _scaleY)
                Circle.key name
                ]
                @props)
        member _.text name (x:float<yards>,y:float<yards>) props =
            Text.create ([
                Text.x (x * _scaleX)
                Text.y (y * _scaleY)
                Text.key name
                ]
                @props)
        member _.group name (x:float<yards>,y:float<yards>) (props: IGroupProperty list) =
            Group.create (([
                Group.x (x * _scaleX)
                Group.y (y * _scaleY)
                Group.key name
                ] : IGroupProperty list)
                @ props)
    let toYard(x:int) = float x * 1.<yards>
    let display (pixelSize: int * int) render =
        stage [
            Stage.height (snd pixelSize)
            Stage.width (fst pixelSize)
            Stage.children [
                Layer.createNamed "background" [
                    Rect.create [
                        Rect.x 0
                        Rect.y 0
                        Rect.fill Color.LightGrey
                        Rect.width (fst pixelSize)
                        Rect.height (snd pixelSize)
                        Rect.key "Rect1"
                        ]
                    ]
                yield! render (RenderHelper(pixelSize))
                ]
            ]
    let layoutGrid (r: RenderHelper) =
        Layer.createNamed "Grid" [
            for x in [0..39] |> List.map toYard do
                for y in [0..39] |> List.map toYard do
                    r.rect $"Rect{x}_{y}" (x, y) [
                        Rect.stroke Color.Black
                        Rect.strokeWidth 1
                        Rect.opacity 0.1
                        Rect.width (r.scaleX 1.<yards>)
                        Rect.height (r.scaleY 1.<yards>)
                        ]
            ]

module private Setup =
    open type Stage
    open type Layer
    open type Circle

    [<ReactComponent>]
    let View (db: Domain.Data.MonsterDatabase) (setup: FightSetup, onDrag) dispatch =
        display (300, 300) <| fun r -> [
            layoutGrid r
            Layer.createNamed "teams" [
                let groups = [
                    let specifics isTeamA (side: GroupSetup list) = [
                        for ix, group in side |> List.mapi Tuple2.create do
                            for n, monsterName in group.members do
                                let c = db.catalog[monsterName]
                                (isTeamA, ix), c.Quantify n, group.center, Domain.CombatRules.radius_ group
                        ]
                    yield! (setup.sideA |> specifics true)
                    match setup.sideB with
                    | Specific sideB -> yield! specifics false sideB
                    | Calibrate ({ members = (Some name, _, _, _) } as group) ->
                        let c = db.catalog[name]
                        (false, 0), $"N {c.PluralName_}", group.center, 5.<yards>
                    | Calibrate _ -> ()
                    ]
                for ((isTeamA, _) as groupAddress: bool * int, label, center, radius) in groups do
                    r.group $"Group{groupAddress}" center [
                        Group.draggable
                        Group.onDragEnd(fun e -> onDrag(groupAddress, (r.unscaleX (e.target.x()), r.unscaleY (e.target.y()))))
                        // Group.offsetX -25
                        // Group.offsetY -25
                        Group.children [|
                            circle [
                                Circle.radius (r.scaleX radius)
                                Circle.fill (if isTeamA then Color.Blue else Color.Purple)
                                Circle.key "outline"
                                ]
                            text [
                                Text.verticalAlign Middle
                                Text.align Center
                                Text.fill Color.Black
                                // do NOT scale text to yards
                                Text.width (50)
                                Text.height (50)
                                Text.offsetX (25)
                                Text.offsetY (25)
                                Text.fontSize 9
                                Text.fontStyle "800" // unusually bold
                                Text.key "name1"
                                Text.text label
                                ]
                            |]

                        ]
                ]
            ]

let Setup = Setup.View
module Actual =

    [<ReactComponent>]
    let View (combatants: Combatant list) dispatch =
        let shownNames, setShownNames = React.useState Map.empty
        let hover, setHover = React.useState None
        display (300, 300) <| fun r -> [
            Layer.createNamed "Background" [
                Rect.create [
                    Rect.x 0
                    Rect.y 0
                    Rect.fill Color.LightGrey
                    Rect.width winW
                    Rect.height winH
                    Rect.key "Rect1"
                    ]
                ]
            layoutGrid r
            Layer.createNamed "combatants" [
                for c in combatants do
                    Group.create ([
                        let x,y = c.coords
                        Group.x (r.scaleX x)
                        Group.y (r.scaleY y)
                        Group.key (toString c.Id)
                        Group.onClick (fun e -> shownNames |> Map.change c.Id (function Some () -> None | None -> Some ()) |> setShownNames)
                        Group.children [
                            circle [
                                Circle.radius (r.scaleX 0.5<yards>)
                                Circle.fill (if c.team = 1 then Color.Blue else Color.Purple)
                                Circle.key "outline"
                                Circle.onMouseEnter (fun e ->
                                    e.target.getStage().container().style.cursor <- CursorType.Pointer
                                    setHover (Some c.Id))
                                Circle.onMouseLeave (fun e ->
                                    if hover = (Some c.Id) then
                                        e.target.getStage().container().style.cursor <- CursorType.Default
                                        setHover None
                                    )
                                if hover = Some c.Id then
                                    Circle.stroke Color.Black
                                    Circle.strokeWidth 2
                                elif shownNames |> Map.containsKey c.Id then
                                    Circle.stroke Color.Black
                                    Circle.strokeWidth 1
                                ]
                            if hover = Some c.Id || shownNames |> Map.containsKey c.Id then
                                text [
                                    Text.verticalAlign Middle
                                    Text.align Center
                                    Text.fill Color.Black
                                    // do NOT scale text to yards
                                    Text.width (120)
                                    Text.height (50)
                                    Text.offsetX (60)
                                    Text.offsetY (35)
                                    Text.fontSize 12
                                    if hover = Some c.Id then Text.fontStyle "900" // unusually bold
                                    else Text.fontStyle "bold"
                                    Text.key "name"
                                    Text.text c.personalName
                                    ]
                            ]
                        ]: IGroupProperty list)
                ]
            ]

let Actual = Actual.View

module private Impl0 =
    open Fable.Core.JsInterop
    let r = System.Random()
    type Movespec = { id: UniqueId; from: int * int; unto: int * int; mutable started: bool; afterwards: unit -> unit }
        with
        member this.start(node: KonvaNode0) =
            if this.started then ()
            else
                this.started <- true
                let id = this.id.ToString().Substring(0,6)
                let square x = x * x
                let length = sqrt (square (fst this.from - fst this.unto) + square (snd this.from - snd this.unto) |> float)
                node.to' (createObj [
                    let x,y = this.unto
                    "x" ==> x
                    "y" ==> y
                    "duration" ==> ((float length) * 0.005 |> min 0.3)
                    "onFinish" ==> (fun () -> this.afterwards())
                    ])
    type Todo =
        | Tween of Movespec
        | Immediate of (unit -> unit)
open Impl0

[<ReactComponent>]
let DefaultFrame (args: FrameInputs) stage =
    Html.div [
        prop.className args.className
        prop.children [
            stage
            class' "control" Html.div [
                let addRandom text _ =
                    Add(Guid.NewGuid(), (r.Next(0, stageW), r.Next(0, stageH)), text) |> args.dispatch
                let jiggle _ =
                    match args.model.creatures.Keys |> List.ofSeq with
                    | [] -> () // nothing to do
                    | ids ->
                        let id = chooseRandom ids
                        Move(id, Relative(r.Next(-250, 250), r.Next(-250, 250))) |> args.dispatch
                let moveDown _ =
                    match args.model.creatures.Keys |> List.ofSeq with
                    | [] -> () // nothing to do
                    | ids ->
                        for id in ids do
                            Move(id, Relative(20, 50)) |> args.dispatch
                let moveToTop _ =
                    match args.model.creatures.Keys |> List.ofSeq with
                    | [] -> () // nothing to do
                    | ids ->
                        for ix, id in ids |> List.mapi Tuple2.create do
                            Move(id, Absolute(100 + ix * 100, 50)) |> args.dispatch
                Html.button [prop.text "Add an orc"; prop.onClick (addRandom "Orc") ]
                Html.button [prop.text "Add an Inigo"; prop.onClick (addRandom "Inigo") ]
                Html.button [prop.text "Random movement"; prop.onClick jiggle ]
                Html.button [prop.text "Move down"; prop.onClick moveDown ]
                Html.button [prop.text "Move To Top"; prop.onClick moveToTop ]
                Html.button [prop.text "Clear"; prop.onClick (fun _ -> args.dispatch Clear) ]
                ]
            ]
        ]


// Arena0 probably won't wind up being used. Consider it a proof of concept.
[<ReactComponent>]
let Arena0 (init, history': Msg list) =
    // start with initialModel, then movements flow in through history' to executionQueue and eventually to canon
    let canon, setCanon = React.useState init
    let futureCanon, setFutureCanon = React.useState init
    let knownHistory, setKnownHistory = React.useState []
    let executionQueue = React.useRef ([]: Todo list) // we need all the closures to share the same mutable queue
    let currentTransition, setCurrentTransition = React.useState None
    let movingObject = React.useRef None
    let (|Inert|Ready|Transitioning|) currentTransition =
        match currentTransition, executionQueue.current with
        | Some m, _ -> Transitioning m
        | None, [] -> Inert
        | None, h::_ -> Ready h
    // because React hooks including useLayoutEffect must happen a fixed number of times, we have to split Tween/Immediate execution between a layout effect and regular code
    // TODO: refactor this logic to be clearer
    let rec pump currentTransition =
        match currentTransition with
        | Transitioning _ -> () // we're already in the middle of a transition, don't start another one
        | Inert -> () // nothing to do
        | Ready (todo) ->
            // move into transitioning state
            executionQueue.current <- executionQueue.current.Tail
            match todo with
            | Immediate f -> f(); pump None
            | Tween _ ->
                setCurrentTransition (Some todo) // will trigger re-render, which will start tween
    React.useLayoutEffect <| fun () ->
        match currentTransition with
        | Transitioning (Tween todo) ->
            match movingObject.current with
            | Some obj->
                todo.start obj
            | v -> shouldntHappen v
        | Inert | Ready _ | Transitioning(Immediate _) | _ -> ()
    let proj model model' = function
        | Clear | Add _ -> Some(Immediate(fun () -> setCanon model')) // we want to set ourselves in the state we'd be AFTER this message
        | Move(id, move) as msg ->
            let c = model.creatures[id] // we want to start at where the creature was BEFORE this message and then move to where it should be AFTER
            { id = id; from = (c.x, c.y); unto = updateViaMovement c move; started = false; afterwards = fun  () -> setCanon model'; setCurrentTransition None }
                |> Tween |> Some
    let futureCanon', todo = CQRS.cqrsDiff update proj (futureCanon, knownHistory) history' // DON'T start the diff from canon, start it from the model we'll have after doing all the messages
        // we don't need the model output, that will come as commands flow through the execution queue
    match todo with
    | [] ->
        match currentTransition with
        | Ready _ -> pump currentTransition
        | _ -> ()
    | h::t ->
        setKnownHistory history'
        setFutureCanon futureCanon'
        // start the pump whenever execution queue goes from empty to non-empty
        let startPump = executionQueue.current.IsEmpty
        executionQueue.current <- executionQueue.current @ todo
        if startPump then pump currentTransition
    Stage.create [
        Stage.width stageW // TODO: there's gotta be a better way to be responsive to mobile size constraints
        Stage.height stageH
        Stage.children [
            Layer.createNamed "background" [
                Rect.create [
                    Rect.x 0
                    Rect.y 0
                    Rect.fill Color.LightGrey
                    Rect.width winW
                    Rect.height winH
                    Rect.key "Rect1"
                    ]
                ]
            Layer.createNamed "arena" [
                for creature in canon.creatures.Values do
                    Group.create [
                        match currentTransition with
                        | Transitioning(Tween { id = id; from = (x,y) }) when id = creature.id ->
                            (Group.x x: IGroupProperty)
                            Group.y y
                            Group.ref (fun node -> movingObject.current <- Some node)
                        | _ ->
                            Group.x creature.x
                            Group.y creature.y
                        Group.key (toString creature.id)
                        Group.children [
                            Circle.create [
                                Circle.radius 25
                                Circle.fill Color.Red
                                Circle.key "circle"
                                Circle.offsetX -25
                                Circle.offsetY -25
                                ]
                            Text.create [
                                Text.verticalAlign Middle
                                Text.align Center
                                Text.fill Color.Black
                                Text.width 50
                                Text.height 50
                                match creature.text with
                                | Some txt -> Text.text txt
                                | None -> ()
                                Text.key "name"
                                ]
                            ]
                        ]
                ]
            ]
        ]


