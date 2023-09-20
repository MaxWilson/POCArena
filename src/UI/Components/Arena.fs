module UI.Components.Arena
open System

type UniqueId = Guid
type Movement = Relative of int * int | Absolute of int * int
type Msg =
    | Add of UniqueId * coords: (int * int) * title: string
    | Move of UniqueId * Movement
    | Clear
type VisualObject = {
    id: UniqueId
    text: string option
    x: int
    y: int
    }
type Model = {
    creatures: Map<UniqueId, VisualObject>
    logReversed: Msg list
    }
type FrameInputs = {
    className: string
    model: Model
    dispatch: Msg -> unit
    }

let winH = Browser.Dom.window.innerHeight |> int
let winW = Browser.Dom.window.innerWidth |> int
let stageH = min 600 (winH - 100) // TODO: there's gotta be a better way to be responsive to mobile size constraints
let stageW = min 800 (winW * 3 / 4) // TODO: there's gotta be a better way to be responsive to mobile size constraints
let init _ =  { creatures = Map.empty; logReversed = [] }
let update msg model =
    let creatures' =
        match msg with
        | Clear -> Map.empty
        | Add(id, coords, title) -> model.creatures |> Map.add id { id = id; x = fst coords; y = snd coords; text = Some title }
        | Move(id, movement) ->
            let changeCoords = function
                | None -> None
                | Some creature ->
                    let x, y =
                        let boundW x = max 0 (min stageW x)
                        let boundH x = max 0 (min stageH x)
                        match movement with
                        | Relative(dx, dy) -> creature.x + dx |> boundW, creature.y + dy |> boundH
                        | Absolute(x, y) -> x, y
                    Some { creature with x = x; y = y }
            model.creatures |> Map.change id changeCoords
    { creatures = creatures'; logReversed = msg::model.logReversed }
