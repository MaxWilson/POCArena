module Main

open Feliz
open Feliz.Router
open UI.Components.Sample
open UI.Components.Arena
open UI.Components.ArenaView
open Browser.Dom
open Fable
open Fable.Core.JsInterop
open Common.UI
importSideEffects "./sass/main.sass"


[<ReactComponent>]
let Router() =
    let (currentUrl, updateUrl) = React.useState(Router.currentUrl())
    React.router [
        router.onUrlChanged updateUrl
        router.children [
            match currentUrl with
            | [ "hello" ] -> Components.HelloWorld()
            | [ "counter" ] -> Components.Counter()
            | otherwise ->
                let state, dispatch = React.useElmishSimple init update
                let frameArgs = {
                    className = "arena"
                    model = state
                    dispatch = dispatch
                    }
                DefaultFrame frameArgs (Arena state)
        ]
    ]

let main() =
    Html.div [
        classP' "srcLink" Html.a [
            prop.href "https://github.com/MaxWilson/POCArena/"
            prop.children [Html.img [prop.src "img/GitHub_Logo.png"]]
            prop.target "_blank"
            ]
        Router()
        ]

let root = ReactDOM.createRoot(document.getElementById "feliz-app")
root.render(main())
