module UI.Konva

open Feliz
open Feliz.Router
open Fable.Core
open Fable.Core.JsInterop

[<Erase>]
type IStageProperty =
    interface end
[<Erase>]
type ILayerProperty =
    interface end
[<Erase>]
type IGroupProperty =
    interface end
[<Erase>]
type ICircleProperty =
    interface end
[<Erase>]
type IRectProperty =
    interface end
[<Erase>]
type ITextProperty =
    interface end
[<Erase>]
type IShapeProperty =
    interface
        inherit IStageProperty
        inherit ILayerProperty
        inherit IGroupProperty
        inherit ICircleProperty
        inherit IRectProperty
        inherit ITextProperty
        end
[<Erase>]
type KonvaNode =
    abstract x : unit -> float
    abstract y : unit -> float

[<AutoOpen>]
module private Interop =
    let inline mkShapeAttr (key: string) (value: obj) : IShapeProperty = unbox (key, value)
    let inline mkStageAttr (key: string) (value: obj) : IStageProperty = unbox (key, value)
    let inline mkLayerAttr (key: string) (value: obj) : ILayerProperty = unbox (key, value)
    let inline mkGroupAttr (key: string) (value: obj) : IGroupProperty = unbox (key, value)
    let inline mkCircleAttr (key: string) (value: obj) : ICircleProperty = unbox (key, value)
    let inline mkRectAttr (key: string) (value: obj) : IRectProperty = unbox (key, value)
    let inline mkTextAttr (key: string) (value: obj) : ITextProperty = unbox (key, value)

    // In React interop scenarios, for stage/layer/group/etc., it's important to use arrays instead of lists so that React won't complain about not having a key
    let inline stage (props: IStageProperty array) = Interop.reactApi.createElement(import "Stage" "react-konva", createObj !!props)
    let inline layer (props: ILayerProperty array) = Interop.reactApi.createElement(import "Layer" "react-konva", createObj !!props)
    let inline group (props: IGroupProperty array) = Interop.reactApi.createElement(import "Group" "react-konva", createObj !!props)
    let inline circle (props: ICircleProperty array) = Interop.reactApi.createElement(import "Circle" "react-konva", createObj !!props)
    let inline rect (props: IRectProperty array) = Interop.reactApi.createElement(import "Rect" "react-konva", createObj !!props)
    let inline text (props: ITextProperty array) = Interop.reactApi.createElement(import "Text" "react-konva", createObj !!props)

[<Erase>]
type Color = Red | Green | Blue | Yellow | Grey | Orange | Purple | LightGrey | DarkGrey | Black

type Shape =
    static member inline key (key:_) = mkShapeAttr "key" key
    static member inline width (w:int) = mkShapeAttr "width" w
    static member inline height (h:int) = mkShapeAttr "height" h
    static member inline width (w:float) = mkShapeAttr "width" w
    static member inline height (h:float) = mkShapeAttr "height" h
    static member inline x (x:int) = mkShapeAttr "x" x
    static member inline y (y:int) = mkShapeAttr "y" y
    static member inline x (x:float) = mkShapeAttr "x" x
    static member inline y (y:float) = mkShapeAttr "y" y
    static member inline pos ((x: int, y:int)) = [Shape.x x; Shape.y y]
    static member inline pos ((x: float, y:float)) = [Shape.x x; Shape.y y]
    static member inline opacity (v:float) = mkShapeAttr "opacity" v
    static member inline fill (color:Color) = mkShapeAttr "fill" color
    static member inline fill (color:string) = mkShapeAttr "fill" color
    static member inline draggable = mkShapeAttr "draggable" true
    static member inline onDragStart (f: ({| target: KonvaNode |} -> 'a)) = mkShapeAttr "onDragStart" f
    static member inline onDragEnd (f: ({| target: KonvaNode |} -> 'a)) = mkShapeAttr "onDragEnd" f
    static member inline onClick (f: ({| target: 'a |} -> 'b)) = mkShapeAttr "onClick" f
    static member inline onMouseDown (f: ({| target: 'a |} -> 'b)) = mkShapeAttr "onMouseDown" f
    static member inline onMouseUp (f: ({| target: 'a |} -> 'b)) = mkShapeAttr "onMouseUp" f
    static member inline onMouseOver (f: ({| target: 'a |} -> 'b)) = mkShapeAttr "onMouseOver" f
    static member inline ref handle = mkShapeAttr "ref" handle
    static member inline offsetX (v: float) = mkShapeAttr "offsetX" v
    static member inline offsetY (v: float) = mkShapeAttr "offsetY" v

type Circle =
    inherit Shape
    static member inline radius (r:float) = mkCircleAttr "radius" r
    static member inline create props = circle (props |> Array.ofList)

[<Erase>]
type LineJoin = Miter | Round | Bevel

type Rect =
    inherit Shape
    static member inline lineJoin (lineJoin:LineJoin) = mkRectAttr "lineJoin" lineJoin
    static member inline strokeWidth (pixels:int) = mkRectAttr "strokeWidth" pixels
    static member inline stroke (color:Color) = mkRectAttr "stroke" color
    static member inline create props = rect (props |> Array.ofList)

[<Erase>]
type VerticalAlign = Top | Middle | Bottom

[<Erase>]
type HorizontalAlign = Left | Center | Right

type Text =
    inherit Shape
    static member inline text (text: string) = mkTextAttr "text" text
    static member inline fontSize (fontSize: int) = mkTextAttr "fontSize" fontSize
    static member inline fontStyle (fontStyle: string) = mkTextAttr "fontStyle" fontStyle
    static member inline align (v: HorizontalAlign) = mkTextAttr "align" v
    static member inline verticalAlign (v: VerticalAlign) = mkTextAttr "verticalAlign" v
    static member inline create props = text (props |> Array.ofList)

type Group =
    inherit Shape
    static member inline children (children: #ReactElement array) = mkGroupAttr "children" children
    static member inline children (children: #ReactElement list) = Group.children (children |> Array.ofList)
    static member inline create (children: #ReactElement list) = group [| Group.children (children |> Array.ofList) |]
    static member inline create (props: IGroupProperty list) = group (props |> Array.ofList)
    static member inline createNamed keyName (children: #ReactElement list) = group [| Group.key keyName |> unbox; Group.children (children |> Array.ofList) |]

type Layer =
    inherit Shape
    static member inline children (children: #ReactElement array) = mkLayerAttr "children" children
    static member inline children (children: #ReactElement list) = Layer.children (children |> Array.ofList)
    static member inline create children = layer [| Layer.children (children |> Array.ofList) |]
    static member inline createNamed keyName (children: #ReactElement list) = layer [| Layer.key keyName |> unbox; Layer.children (children |> Array.ofList) |]

type Stage =
    inherit Shape
    static member inline children (children: #ReactElement array) = mkStageAttr "children" children
    static member inline children (children: _ list) = Stage.children (children |> Array.ofList)
    static member inline create (props: IStageProperty list) = stage (props |> Array.ofList)
    static member inline create (props, children) = Stage.create ((Stage.children (children |> Array.ofList))::props)

type KonvaNode0 =
    [<Emit "$0.to($1)">]
    member inline this.to' (args:obj) : unit = jsNative

let stage = Stage.create
let layer = Layer.create
let inline group (children: IGroupProperty list) = Group.create children
let circle = Circle.create
let rect = Rect.create
let text = Text.create
