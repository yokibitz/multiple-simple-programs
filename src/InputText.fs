module InputText

open Feliz
open Elmish

type State =
    { InputText: string
      IsUpperCase: bool }

type Msg =
    | InputTextChanged of string
    | UpperCaseToggled of bool

let init () =
    { InputText = ""; IsUpperCase = false }, Cmd.none

let update msg state =
    match msg with
    | InputTextChanged text -> { state with InputText = text }, Cmd.none
    | UpperCaseToggled upperCase -> { state with IsUpperCase = upperCase }, Cmd.none

let render state dispatch =
    Html.div
        [ Html.input
            [ prop.valueOrDefault state.InputText
              prop.onChange (InputTextChanged >> dispatch) ]

          Common.divider

          Html.input
              [ prop.id "uppercase-checkbox"
                prop.type'.checkbox
                prop.isChecked state.IsUpperCase
                prop.onChange (UpperCaseToggled >> dispatch) ]

          Html.label
              [ prop.htmlFor "uppercase-checkbox"
                prop.text "Uppercase" ]

          Html.h3 (if state.IsUpperCase then state.InputText.ToUpper() else state.InputText) ]
