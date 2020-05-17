module App

open Elmish
open Elmish.React
open Feliz

[<RequireQualifiedAccess>]
type Page =
    | Counter
    | TextInput

type State =
    { Counter: Counter.State
      InputText: InputText.State
      CurrentPage: Page }

type Msg =
    | CounterMsg of Counter.Msg
    | InputTextMsg of InputText.Msg
    | SwitchPage of Page


let init () =
    { Counter = Counter.init ()
      InputText = InputText.init ()
      CurrentPage = Page.Counter },
    Cmd.none


let update (msg: Msg) (state: State) =
    match msg with
    | CounterMsg counterMsg ->
        let counter, counterCmd = Counter.update counterMsg state.Counter

        let appCmd = Cmd.map CounterMsg counterCmd

        { state with Counter = counter }, appCmd

    | InputTextMsg inputTextMsg ->
        let inputText =
            InputText.update inputTextMsg state.InputText

        { state with InputText = inputText }, Cmd.none

    | SwitchPage page -> { state with CurrentPage = page }, Cmd.none


let render (state: State) (dispatch: Msg -> unit) =

    match state.CurrentPage with
    | Page.Counter ->
        Html.div
            [ Html.button
                [ prop.text "Show Text Input"
                  prop.onClick (fun _ -> dispatch (SwitchPage Page.TextInput)) ]

              Common.divider
              Counter.render state.Counter (CounterMsg >> dispatch) ]

    | Page.TextInput ->
        Html.div
            [ Html.button
                [ prop.text "Show counter"
                  prop.onClick (fun _ -> dispatch (SwitchPage Page.Counter)) ]

              Common.divider
              InputText.render state.InputText (InputTextMsg >> dispatch) ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
