module UI.Components.ArenaView

open System
open UI.Konva
open Feliz
open Feliz.UseElmish
open Elmish
open UI.Components.Arena

module private Setup =
    open type Stage
    open type Layer
    open type Circle
    [<ReactComponent>]
    let View (db: Domain.Data.MonsterDatabase) (teams: (int * ((int * string) list)) list) dispatch =
        stage [
            Stage.height 200
            Stage.width 300
            Stage.children [
                layer [
                    Rect.create [
                        Rect.x 0
                        Rect.y 0
                        Rect.fill Color.LightGrey
                        Rect.width winW
                        Rect.height winH
                        Rect.key "Rect1"
                        ]
                    ]
                layer [
                    let teamPositions = [1, (50, 45); 2, (250, 115)] |> Map.ofList
                    for team, groups in teams do
                        let x,y = teamPositions[team]
                        group [
                            Group.x x
                            Group.y y
                            // Group.offsetX -25
                            // Group.offsetY -25
                            Group.children [
                                circle [
                                    Circle.radius 40
                                    Circle.fill Color.Red
                                    // Circle.offsetX -25
                                    // Circle.offsetY -25
                                    ]
                                text [
                                    Text.verticalAlign Middle
                                    Text.align Center
                                    Text.fill Color.Black
                                    Text.width 80
                                    Text.height 50
                                    Text.offsetX 40
                                    Text.offsetY 25
                                    Text.fontSize 9
                                    Text.fontStyle "800" // unusually bold
                                    let txt =
                                        [   for n, monsterName in groups do
                                                let c = db.catalog[monsterName] in if n = 1 then c.name else c.PluralName_
                                            ]
                                        |> String.join ", "

                                    Text.text (txt)
                                    ]
                                ]

                            ]
                    ]
                ]
            ]
let Setup = Setup.View

module private Impl =
    open Fable.Core.JsInterop
    let r = System.Random()
    type Movespec = { id: UniqueId; from: int * int; unto: int * int; mutable started: bool; afterwards: unit -> unit }
        with
        member this.start(node: KonvaNode) =
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
open Impl

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


