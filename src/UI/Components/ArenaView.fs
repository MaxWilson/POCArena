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
                "duration" ==> (float length) * 0.05
                "finish" ==> (fun () -> this.afterwards())
                ])
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
let Arena (initialModel, history': Msg list) =
    // start with initialModel, then movements flow in through history' to executionQueue and eventually to canon
    let canon, setCanon = React.useState initialModel
    let movingCircle = React.useRef None
    let movingText = React.useRef None
    let executionQueue, setExecutionQueue = React.useState []
    let _, todo = CQRS.cqrsDiff update (fun model -> function | Clear | Add _ -> None | Move(id, move) as msg -> Some (let c = model.creatures[id] in { id = id; from = (c.x, c.y); unto = updateViaMovement c move; afterwards = fun tailQueue () -> setCanon model; pump tailQueue })) (canon, []) history'
        // we don't want the model output from cqrsDiff because it's going to flow through executionQueue via afterwards()
    let pump queue =
        React.useLayoutEffect (fun () ->
            match queue with
            | [] -> ()
            | todo::tail ->
                let node = movingCircle.current.Value
                let node' = movingText.current.Value
                head.start node
                head.start node'
                setExecutionQueue tail
            )
    let executionQueue =
        match todo with
        | [] -> executionQueue
        | h::t ->
            // start the pump whenever execution queue goes from empty to non-empty
            if executionQueue.IsEmpty then
                pump todo
            let executionQueue' = executionQueue @ todo
            setExecutionQueue executionQueue'
            executionQueue'

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
                        match executionQueue with
                        | head::tail when head.id = creature.id ->
                            Circle.x (head.from |> fst |> float)
                            Circle.y (head.from |> snd |> float)
                            ("ref", (fun (n:KonvaNode) -> movingCircle.current <- Some n)) |> unbox
                        | _ ->
                            Circle.x (creature.x |> float)
                            Circle.y (creature.y |> float)
                        Circle.radius 25
                        Circle.fill Color.Red
                        Circle.key ("circle" + toString creature.id)
                        ]
                    text [
                        match executionQueue with
                        | head::tail when head.id = creature.id ->
                            Circle.x (head.from |> fst |> float)
                            Circle.y (head.from |> snd |> float)
                            ("ref", (fun (n:KonvaNode) -> movingText.current <- Some n)) |> unbox
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

