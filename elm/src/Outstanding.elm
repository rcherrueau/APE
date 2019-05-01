module Outstanding exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json

import Result.Extra as Result

import Material.Options as Options

{-| Convenience helpers for working with outstanding inputs.

An outstanding input is a sort of input that might be wrong while you
type it (e.g., during an `onInput`), but should be committed to a
certain type at the end (e.g., during an `onChange`). The outstanding
input is stored as a `String` in the left part (i.e. `Err`) of a
`Result`. And then committed input is stored in the right part (i.e.,
`Ok`).

My common usage for such a data type is for `Html.input` textfield.
The input in the view is linked to an Outstanding input in the model.
An HTML events `onOutstandingInput` always decodes as String and put
results in the left part of the Outstanding input. While an HTML
events `onOutstandingChange` commits the value to the right part (or
keep it to the left if the commitment fails).

    type alias Model =
        { myValue: Outstanding Float }

    type Msg = UpdateMyValue (Outstanding Float)

    view m =
      Html.input
        [ Html.Attributes.type_ "text"
        , Html.Attributes.value (showOutstanding fromFloat m.myValue)
        , onOutstandingInput  UpdateMyValue         -- Put result in left side
        , onOutstandingChange toFloat UpdateMyValue -- Try to put result in rigth side
        ] []
-}
type alias Outstanding t = Result String t
type alias OutstandingFloat = Outstanding Float

stopPropagation : a -> (a, Bool)
stopPropagation a = (a, True)

onOutstandingInput : (Outstanding t -> msg) -> Attribute msg
onOutstandingInput mkMsg =
    stopPropagationOn "input" <|
        Json.map (stopPropagation << mkMsg << Err) theValue

onOutstandingChange : (String -> Maybe t) -> (Outstanding t -> msg) -> Attribute msg
onOutstandingChange f mkMsg =
    stopPropagationOn "change" <|
        Json.map (stopPropagation << mkMsg << Result.fromMaybe "error" << f) theValue

onOutstanding: (String -> Maybe t) -> (Outstanding t -> msg) -> List (Attribute msg)
onOutstanding f mkMsg =
    [ onOutstandingInput mkMsg , onOutstandingChange f mkMsg ]


--------------------------------------------------------------------------------
-- Mdc
stopPropagationMdc : m -> { message : m, stopPropagation : Bool, preventDefault : Bool }
stopPropagationMdc m = { message = m, stopPropagation = True, preventDefault = False }

onOutstandingInputMdc : (Outstanding t -> msg) -> Options.Property c msg
onOutstandingInputMdc f =
    Options.onWithOptions "input" <|
        Json.map (stopPropagationMdc << f << Err) theValue

onOutstandingChangeMdc : (String -> Maybe t) -> (Outstanding t -> msg) -> Options.Property c msg
onOutstandingChangeMdc g f =
    Options.onWithOptions "change" <|
        Json.map (stopPropagationMdc << f << Result.fromMaybe "error" << g) theValue

onOutstandingMdc : (String -> Maybe t) -> (Outstanding t -> msg) -> List (Options.Property c msg)
onOutstandingMdc f mkMsg =
    [ onOutstandingInputMdc mkMsg , onOutstandingChangeMdc f mkMsg ]

--------------------------------------------------------------------------------
-- Utils
targetTextContent : Json.Decoder String
targetTextContent = Json.at ["target", "textContent"] Json.string

theValue : Json.Decoder String
theValue = Json.oneOf [ targetValue, targetTextContent ]

showOutstanding : (t -> String) -> Outstanding t -> String
showOutstanding f = Result.unpack identity f

{-| Returns `Just t` if String is empty.

Provides a default value if the string is empty.

    (String.toFloat |> withDefault 0.42) ""      -- Just 0.42
    (String.toFloat |> withDefault 0.42) "0.84"  -- Just 0.84
    (String.toFloat |> withDefault 0.42) "foo"   -- Nothing
-}
withDefault : t -> (String -> Maybe t) -> String -> Maybe t
withDefault t f s = if String.isEmpty s then Just t else f s
