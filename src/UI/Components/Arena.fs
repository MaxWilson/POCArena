module UI.Components.Arena

open Common.UI
open UI.Konva
open Feliz

//[<ReactComponent>]
let Arena () =
    let spanWrap (name: string) e = Html.span [prop.className name; prop.children [e]]
    let divWrap (className: string) element =
            Html.div [
                prop.className className
                prop.children [element; Html.button [prop.text "OK"]]
                ]
    divWrap "arena" <| stage [
        Stage.width 800
        Stage.height 600
        Stage.children [
            Layer.create "arena" [
                rect [
                    Rect.x 0
                    Rect.y 0
                    Rect.fill Color.LightGrey
                    Rect.width 800
                    Rect.height 600
                    ]
                circle [
                    Shape.x 100
                    Shape.y 100
                    Shape.width 50
                    Shape.height 50
                    Shape.fill Color.Red
                    Shape.key "circle1"
                    ]
                rect [
                    Shape.x 200
                    Shape.y 200
                    Shape.width 50
                    Shape.height 50
                    Shape.fill Color.Green
                    Shape.key "rect1"
                    ]
                text [
                    Shape.x 300
                    Shape.y 300
                    Text.text "Hello World"
                    Shape.fill Color.Blue
                    Shape.key "text1"
                    ]
                ]
            ]
        ]

