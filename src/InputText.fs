module InputText

open Feliz

type State =
    { InputText: string
      IsUpperCase: bool }

type Msg =
    | InputTextChanged of string
    | UpperCaseToggled of bool

let init () = { InputText = ""; IsUpperCase = false }

let update msg state =
    match msg with
    | InputTextChanged text -> { state with InputText = text }
    | UpperCaseToggled upperCase -> { state with IsUpperCase = upperCase }

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
