module UI.Components.ArenaView

open System
open Common.UI
open UI.Konva
open Feliz
open Feliz.UseElmish
open Elmish
open UI.Components.Arena
module private Impl =
    open Fable.Core.JsInterop
    let r = System.Random()

    type Movespec = { id: UniqueId; from: int * int; unto: int * int; afterwards: unit -> unit }
        with
        member this.start(node: KonvaNode) =
            let square x = x * x
            let length = sqrt (square (fst this.from - fst this.unto) + square (snd this.from - snd this.unto) |> float)
            node.to' (createObj [
                "x" ==> fst this.unto
                "y" ==> snd this.unto
                "duration" ==> 1.0 // (float length) * 0.05
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
                        Move(id, Relative(r.Next(-50, 50), r.Next(-50, 50))) |> args.dispatch
                Html.button [prop.text "Add an orc"; prop.onClick (addRandom "Orc") ]
                Html.button [prop.text "Add an Inigo"; prop.onClick (addRandom "Inigo") ]
                Html.button [prop.text "Random movement"; prop.onClick jiggle ]
                Html.button [prop.text "Clear"; prop.onClick (fun _ -> args.dispatch Clear) ]
                ]
            ]
        ]


[<ReactComponent>]
let Arena (init, history': Msg list) =
    // start with initialModel, then movements flow in through history' to executionQueue and eventually to canon
    let canon, setCanon = React.useState init
    let knownHistory, setKnownHistory = React.useState []
    let executionQueue = React.useRef ([]: Todo list) // we need all the closures to share the same mutable queue
    let movingCircle = React.useRef None
    let movingText = React.useRef None
    // because React hooks including useLayoutEffect must happen a fixed number of times, we have to split Tween/Immediate execution between a layout effect and regular code
    // TODO: refactor this logic to be clearer
    let rec pump () =
        match executionQueue.current with
        | [] -> () // nothing to do
        | Immediate(todo)::tail ->
            todo()
            executionQueue.current <- tail
            pump()
        | (Tween todo)::tail -> ()
            // pump() will happen from inside the Tween that's started in useLayoutEffect
    React.useLayoutEffect <| fun () ->
        match executionQueue.current with
        | [] -> () // nothing to do
        | Immediate(todo)::tail -> ()
        | (Tween todo)::tail ->
            match movingCircle.current, movingText.current with
            | Some circle, Some text ->
                todo.start circle
                todo.start text
                executionQueue.current <- tail
            | v -> shouldntHappen v
    let proj model = function
        | Clear | Add _ -> Some(Immediate(fun () -> setCanon model))
        | Move(id, move) as msg ->
            let c = model.creatures[id]
            { id = id; from = (c.x, c.y); unto = updateViaMovement c move; afterwards = fun  () -> setCanon model; pump() }
                |> Tween |> Some
    let _, todo = CQRS.cqrsDiff update proj (canon, knownHistory) history'
        // we don't need the model output, that will come as commands flow through the execution queue
    //if todo.Length > 0 then printfn $"{knownHistory} ==> {history'} yields TODO queue: {todo}"
    match todo with
    | [] -> ()
    | h::t ->
        setKnownHistory history'
        // start the pump whenever execution queue goes from empty to non-empty
        let startPump = executionQueue.current.IsEmpty
        executionQueue.current <- executionQueue.current @ todo
        if startPump then pump()
    stage [
        Stage.width stageW // TODO: there's gotta be a better way to be responsive to mobile size constraints
        Stage.height stageH
        Stage.children [
            Layer.create "background" [
                rect [
                    Rect.x 0
                    Rect.y 0
                    Rect.fill Color.LightGrey
                    Rect.width winW
                    Rect.height winH
                    Rect.key "Rect1"
                    ]
                ]
            Layer.create "arena" [
                for creature in canon.creatures.Values do
                    circle [
                        match executionQueue.current with
                        | (Tween head)::tail when head.id = creature.id ->
                            Circle.x (head.from |> fst |> float)
                            Circle.y (head.from |> snd |> float)
                            Circle.ref (fun (n:KonvaNode) -> movingCircle.current <- Some n)
                        | _ ->
                            Circle.x (creature.x |> float)
                            Circle.y (creature.y |> float)
                        Circle.radius 25
                        Circle.fill Color.Red
                        Circle.key ("circle" + toString creature.id)
                        ]
                    text [
                        match executionQueue.current with
                        | (Tween head)::tail when head.id = creature.id ->
                            Text.x (head.from |> fst |> float)
                            Text.y (head.from |> snd |> float)
                            Text.ref (fun (n:KonvaNode) -> movingText.current <- Some n)
                        | _ ->
                            Text.x (creature.x |> float)
                            Text.y (creature.y |> float)
                        Text.verticalAlign Middle
                        Text.align Center
                        Text.fill Color.Black
                        Text.width 50
                        Text.height 50
                        match creature.text with
                        | Some txt -> Text.text txt
                        | None -> ()
                        Text.key ("txt" + toString creature.id)
                        ]
                ]
            ]
        ]

