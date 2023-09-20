module UI.Components.ArenaView

open System
open Common.UI
open UI.Konva
open Feliz
open Feliz.UseElmish
open Elmish
open UI.Components.Arena

module private Impl =
    let r = System.Random()
open Impl

[<ReactComponent>]
let DefaultFrame (args: FrameInputs) stage =
    Html.div [
        prop.className args.className
        prop.children [
            stage
            class' "control" Html.div [
                let addRandom text _ =
                    Add(Guid.NewGuid(), (r.Next(0, 800), r.Next(0, 600)), text) |> args.dispatch
                let jiggle _ =
                    match args.model.Keys |> List.ofSeq with
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
let Arena (frame: FrameInputs -> ReactElement -> ReactElement) =
    let init _ =  Map.empty
    let update msg model =
        match msg with
        | Clear -> Map.empty
        | Add(id, coords, title) -> model |> Map.add id { id = id; x = fst coords; y = snd coords; text = Some title }
        | Move(id, movement) ->
            let changeCoords = function
                | None -> None
                | Some creature ->
                    let x, y =
                        match movement with
                        | Relative(dx, dy) -> creature.x + dx, creature.y + dy
                        | Absolute(x, y) -> x, y
                    Some { creature with x = x; y = y }
            model |> Map.change id changeCoords
    let (state: Map<_, _>), dispatch = React.useElmishSimple init update
    let frameArgs = {
        className = "arena"
        model = state
        dispatch = dispatch
        }
    // "objects" in the game design sense, not the OOP sense
    frame frameArgs <| stage [
        Stage.width 800
        Stage.height 600
        Stage.children [
            Layer.create "background" [
                rect [
                    Rect.x 0
                    Rect.y 0
                    Rect.fill Color.LightGrey
                    Rect.width 800
                    Rect.height 600
                    Rect.key "Rect1"
                    ]
                ]
            Layer.create "arena" [
                for creature in state.Values do
                    let x,y = creature.x, creature.y
                    circle [
                        Circle.x x
                        Circle.y y
                        Circle.radius 25
                        Circle.fill Color.Red
                        Circle.key ("circle" + toString creature.id)
                        ]
                    text [
                        Text.x (x - 25)
                        Text.y (y - 25)
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

