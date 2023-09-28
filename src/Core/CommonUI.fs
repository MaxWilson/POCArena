[<AutoOpen>]
module CommonUI

open Feliz
open Elmish
open Feliz.UseElmish
let class' (className: string) ctor (elements: ReactElement seq) = ctor [prop.className className; prop.children elements]
let classP' (className: string) ctor (elements: IReactProperty list) : ReactElement = ctor ((prop.className className)::elements)
let classTxt' (className: string) ctor (txt: string) = ctor [(prop.className className); prop.text txt]
let divWrap (className: string) element =
    Html.div [
        prop.className className
        prop.children [element]
        ]

exception UserFacingException of msg:string
let informUserOfError msg = UserFacingException msg |> raise

type React =
    static member inline useElmishSimple (init: _ -> 'model) (update: 'msg -> 'model -> 'model) =
        React.useElmish(fun _ -> Program.mkSimple init update (fun _ _ -> ()))
