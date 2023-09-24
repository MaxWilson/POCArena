module Main

open Feliz
open Feliz.Router
open Feliz.UseListener
open UI.Components.Sample
open UI.Components.AutoFightView
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
    let state, dispatch = React.useElmishSimple init update
    React.router [
        router.onUrlChanged updateUrl
        router.children [
            let header selected =
                class' "header" Html.div [
                    for link, dest in [ "Autofight", "autofight"; "Interactive", "arena"; "Adventure", "adventure"; "Campaign", "campaign" ] do
                        if selected = link then
                            classP' "internalLink" Html.b [ prop.children [Html.text link] ]
                        elif (["Adventure"; "Campaign"] |> List.contains link) then // we want to give an early warning BEFORE changing the URL
                            classP' "internalLink" Html.a [prop.href ("#" + dest); prop.children [Html.text link]; prop.onClick (fun ev -> ev.preventDefault(); notImpl $"{link} mode" )]
                        else classP' "internalLink" Html.a [prop.href ("#" + dest); prop.children [Html.text link] ]
                    classP' "srcLink" Html.a [
                        prop.href "https://github.com/MaxWilson/POCArena/"
                        prop.children [Html.img [prop.src "img/GitHub_Logo.png"]]
                        prop.target "_blank"
                        ]
                    ]
            match currentUrl with
            | [ "hello" ] -> Components.HelloWorld()
            | [ "counter" ] -> Components.Counter()
            | [ "both" ] -> Components.HelloWorld(); Components.Counter()
            | [ "arena" ] ->
                let frameArgs = {
                    className = "arena"
                    model = state
                    dispatch = dispatch
                    }
                header "Interactive"
                DefaultFrame frameArgs (Arena (init, state.history))
            | [ "adventure" ] -> notImpl "Adventure mode"
            | [ "campaign" ] -> notImpl "Campaign mode"
            | otherwise ->
                header "Autofight"
                AutoFight()
            ]
        ]

// originally from https://github.com/fable-compiler/fable-react/blob/master/docs/react-error-boundaries.md, but updated to Fable 4
module ReactErrorBoundary =
    open Fable.Core
    open Fable.React
    open Fable.Core.JsInterop

    [<ReactComponent>]
    let WindowProtector(child) =
        let error, setError = React.useState None
        React.useWindowListener.onError(fun (ev: Browser.Types.UIEvent) -> setError (Some (ev?message)))
        match error with
        | Some error ->
            class' "error" Html.div [
                Html.text $"There has been an error: {error}"
                Html.div [
                    Html.button [ prop.onClick (fun _ -> setError None); prop.children [Html.text "Dismiss"] ]
                    ]
                ]
        | None -> child


    type [<AllowNullLiteral>] InfoComponentObject =
        abstract componentStack: string with get

    type ErrorBoundaryProps =
        {   Inner : React.ReactElement
            ErrorComponent : string -> (unit -> unit) -> React.ReactElement
            OnError : exn * InfoComponentObject -> unit }

    type ErrorBoundaryState =
        { Error : string option }

    // See https://github.com/MangelMaxime/Fulma/blob/master/docs/src/Widgets/Showcase.fs
    // See https://reactjs.org/docs/error-boundaries.html
    type ErrorBoundary(props) =
        inherit React.Component<ErrorBoundaryProps, ErrorBoundaryState>(props)
        do base.setInitState({ Error = None })

        override this.componentDidCatch(error, info) =
            let info = info :?> InfoComponentObject
            this.props.OnError(error, info)
            this.setState(fun _ _ -> { Error = Some (error.ToString()) })

        override this.render() =
            let clearError = fun () -> this.setState(fun _ _ -> { Error = None })
            match this.state.Error with
            | Some err ->
                this.props.ErrorComponent err clearError
            | None ->
                WindowProtector(this.props.Inner)

    // let ofType props children =
    //     ReactElementType.create ReactElementType.ofComponent<'T,_,_> props children
    let renderCatchSimple errorElement element =
        ReactElementType.create ReactElementType.ofComponent<ErrorBoundary,_,_> { Inner = element; ErrorComponent = errorElement; OnError = fun _ -> () } [ ]

    let renderCatchFn onError errorElement element =
        ReactElementType.create ReactElementType.ofComponent<ErrorBoundary,_,_> { Inner = element; ErrorComponent = errorElement; OnError = onError } [ ]

let main() =
    let err msg clearError =
        class' "error" Html.div [
            Html.text $"There has been an error: {msg}"
            Html.div [
                Html.button [ prop.onClick (fun _ -> clearError()); prop.children [Html.text "Dismiss"] ]
                ]
            ]
    ReactErrorBoundary.renderCatchSimple err <|
        Router()

let root = ReactDOM.createRoot(document.getElementById "feliz-app")
root.render(Html.div [ main() ])
