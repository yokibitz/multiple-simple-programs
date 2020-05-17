module App

open Elmish
open Elmish.React
open Feliz

[<RequireQualifiedAccess>]
type Page =
    | Counter of Counter.State
    | InputText of InputText.State

type State = { CurrentPage: Page }

type Msg =
    | CounterMsg of Counter.Msg
    | InputTextMsg of InputText.Msg
    | SwitchToCounter of count: int
    | SwitchToInputText


let init () =
    let initialPage, initialCmd = Counter.init 0
    { CurrentPage = Page.Counter initialPage }, initialCmd



let update (msg: Msg) (state: State) =
    match state.CurrentPage, msg with
    | Page.Counter counterState, CounterMsg counterMsg ->
        let counterState, counterCmd = Counter.update counterMsg counterState

        let nextState =
            { state with
                  CurrentPage = Page.Counter counterState }

        let nextCmd = Cmd.map CounterMsg counterCmd
        nextState, nextCmd
    | Page.InputText inputTextState, InputTextMsg inputTextMsg ->
        let inputTextState, inputTextCmd =
            InputText.update inputTextMsg inputTextState

        let nextState =
            { state with
                  CurrentPage = Page.InputText inputTextState }

        let nextCmd = Cmd.map InputTextMsg inputTextCmd
        nextState, nextCmd
    | _, SwitchToCounter count ->
        let counterState, counterCmd = Counter.init count

        let nextState =
            { state with
                  CurrentPage = Page.Counter counterState }

        let nextCmd = Cmd.map CounterMsg counterCmd
        nextState, nextCmd
    | _, SwitchToInputText ->
        let inputTextState, inputTextCmd = InputText.init ()

        let nextState =
            { state with
                  CurrentPage = Page.InputText inputTextState }

        let nextCmd = Cmd.map InputTextMsg inputTextCmd
        nextState, nextCmd
    | _, _ -> state, Cmd.none

let render (state: State) (dispatch: Msg -> unit) =

    match state.CurrentPage with
    | Page.Counter counterState ->
        Html.div
            [ Html.button
                [ prop.text "Show Text Input"
                  prop.onClick (fun _ -> dispatch SwitchToInputText) ]

              Common.divider

              Counter.render counterState (CounterMsg >> dispatch) ]

    | Page.InputText inputTextState ->
        Html.div
            [ Html.button
                [ prop.text "Show counter"
                  prop.onClick (fun _ -> 0 |> SwitchToCounter |> dispatch) ]

              Common.divider
              InputText.render inputTextState (InputTextMsg >> dispatch) ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
