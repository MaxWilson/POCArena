module UI.Components.Arena
open System

type UniqueId = Guid
type Movement = Relative of int * int | Absolute of int * int
type Msg =
    | Add of UniqueId * coords: (int * int) * title: string
    | Move of UniqueId * Movement
type VisualObject = {
    id: UniqueId
    text: string option
    x: int
    y: int
    }
type FrameInputs = {
    className: string
    model: Map<UniqueId, VisualObject>
    dispatch: Msg -> unit
    }
