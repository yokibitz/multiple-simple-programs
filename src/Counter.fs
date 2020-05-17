[<RequireQualifiedAccess>]
module Counter

open Feliz
open Elmish

type State = { Count: int }

type Msg =
    | Increment
    | Decrement
    | IncrementDelayed

let init () = { Count = 0 }

let update msg state =
    match msg with
    | Increment -> { state with Count = state.Count + 1 }, Cmd.none
    | Decrement -> { state with Count = state.Count - 1 }, Cmd.none
    | IncrementDelayed ->
        let delayedIncrement =
            async {
                do! Async.Sleep 1000
                return Increment
            }

        state, Cmd.fromAsync delayedIncrement

let render state dispatch =
    Html.div
        [ Html.button
            [ prop.onClick (fun _ -> dispatch Increment)
              prop.text "Increment" ]

          Html.button
              [ prop.onClick (fun _ -> dispatch Decrement)
                prop.text "Decrement" ]

          Html.button
              [ prop.onClick (fun _ -> dispatch IncrementDelayed)
                prop.text "Increment Delayed" ]

          Html.h1 state.Count ]
