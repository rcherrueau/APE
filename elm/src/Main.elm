import Dict exposing (Dict)

import Browser exposing (element)
import Browser.Dom exposing (focus)
import Browser.Navigation exposing (reloadAndSkipCache)
import Html exposing (Html, text)
import Html.Attributes exposing (id)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Task

import Result exposing (andThen)
import Result.Extra as Result

import FormatNumber exposing (format)
import FormatNumber.Locales exposing (frenchLocale)

import Material
import Material.Button as Button
import Material.Checkbox as Checkbox
import Material.Elevation as Elevation
import Material.FormField as FormField
import Material.Icon as Icon
import Material.LayoutGrid as LayoutGrid
import Material.Options as Options exposing (when)
import Material.TextField as Textfield
import Material.TextField.HelperText as Textfield
import Material.Theme as Theme
import Material.TopAppBar as TopAppBar

import Outstanding exposing (..)

main =
    Browser.element
      { init = init
      , update = update
      , view = view
      , subscriptions = subscriptions
      }

-- MODEL
type alias Model =
    { mdc: Material.Model Msg
    , newEntry: Entry
    , entries: Dict Int Entry
    , coefMin: Outstanding Float
    , coefMax: Outstanding Float
    , shipFactor: Outstanding Float
    , currentUid: Int
    }

type alias Entry =
    { quantity: Outstanding Float
    , price: Outstanding Float
    , isShipped: Bool
    }

defaultCoefMin : Float
defaultCoefMin = 0.8

defaultCoefMax : Float
defaultCoefMax = 0.7

defaultShipFactor : Float
defaultShipFactor = 3

defaultNewEntry : Entry
defaultNewEntry = (Entry (Err "") (Err "") True)

nanMessage : String
nanMessage = "✗ Nombre"


entriesMoc = Dict.fromList
          [ (0, Entry (Ok 1) (Ok 1) False)
          , (1, Entry (Ok 1) (Ok 1) True)
          , (2, Entry (Ok 2) (Ok 1) False)
          , (3, Entry (Ok 3) (Ok 1) False)
          , (4, Entry (Ok 4) (Ok 1) False)
          ]

init : () -> (Model, Cmd Msg)
init _ =
    ( Model
          Material.defaultModel
          defaultNewEntry
          entriesMoc
          -- Dict.empty
          (Ok defaultCoefMin)
          (Ok defaultCoefMax)
          (Ok defaultShipFactor)
          0
    , Material.init Mdc
    )

subscriptions : Model -> Sub Msg
-- subscriptions = always Sub.none
subscriptions model =
    Material.subscriptions Mdc model

-- Helper
totalQuantity : Model -> Outstanding Float
totalQuantity = Result.map List.sum
                    << Result.combine
                    << List.map .quantity
                    << Dict.values << .entries

computePrice : Float -> Entry -> Outstanding Float
computePrice shipFactor {quantity, price, isShipped} =
    quantity |> andThen (\q ->
    price    |> andThen (\p ->
      let computedPrice = q * p
          factorPrice   = computedPrice * (if isShipped then shipFactor / 100 else 0)
      in Ok (computedPrice + factorPrice)))

priceMean : Model -> Outstanding Float
priceMean m =
    m.shipFactor
    |> andThen (\shipFactor ->
       let cPrice  = computePrice shipFactor
       in (Result.combine <| List.map cPrice <| Dict.values <| m.entries)
    |> andThen (\prices -> totalQuantity m
    |> andThen (\totalQ ->
       let result = List.sum prices / totalQ
       in if isNaN result then Err "NaN" else Ok result)))

priceMeanMin : Model -> Outstanding Float
priceMeanMin m =
    priceMean m
    |> andThen (\pMean -> m.coefMin
    |> andThen (\coefMin -> Ok (pMean / coefMin)))

priceMeanMax : Model -> Outstanding Float
priceMeanMax m =
    priceMean m
    |> andThen (\pMean -> m.coefMax
    |> andThen (\coefMax -> Ok (pMean / coefMax)))

-- UPDATE
type Msg
    = Mdc (Material.Msg Msg)

    -- new-entry
    | UpdateNewEntryQuantity (Outstanding Float)
    | UpdateNewEntryPrice (Outstanding Float)
    | ToggleNewEntryIsShipped
    | AddNewEntry

    -- entry
    | ToggleEntryIsShipped Int
    | RemoveEntry Int

    -- params
    | UpdateCoefMin (Outstanding Float)
    | UpdateCoefMax (Outstanding Float)
    | UpdateShipFactor (Outstanding Float)
    | ReloadPage

    -- Error
    | Focus (Result Browser.Dom.Error ())


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Mdc msg_ ->
            Material.update Mdc msg_ model

        -- new-entry:
        UpdateNewEntryQuantity n ->
            let newEntry     = model.newEntry
                updatedEntry = { newEntry | quantity = n }
            in ({ model | newEntry = updatedEntry }, Cmd.none)

        UpdateNewEntryPrice n ->
            let newEntry     = model.newEntry
                updatedEntry = { newEntry | price = n }
            in ({ model | newEntry = updatedEntry }, Cmd.none)

        ToggleNewEntryIsShipped ->
            let newEntry        = model.newEntry
                toggleIsShipped = not newEntry.isShipped
                updatedEntry    = { newEntry | isShipped = toggleIsShipped }
            in ({ model | newEntry = updatedEntry }, Cmd.none)

        AddNewEntry ->
            let entries     = model.entries
                newEntryUid = model.currentUid
                newEntryVal = Entry model.newEntry.quantity
                                    model.newEntry.price
                                    model.newEntry.isShipped
            in ({ model | entries    = Dict.insert newEntryUid newEntryVal entries
                        , currentUid = newEntryUid + 1
                        , newEntry   = defaultNewEntry -- Reset newEntry to its
                                                       -- default state
                }, Task.attempt Focus                  -- and,
                    (focus "new-entry:quantity"))      -- Focus on quantity field

        -- entry-uid:
        ToggleEntryIsShipped uid ->
            case Dict.get uid model.entries of
                Just  e ->
                  let toggleIsShipped = not e.isShipped
                      newE            = { e | isShipped = toggleIsShipped }
                  in ({ model | entries = Dict.insert uid newE model.entries }, Cmd.none)
                Nothing -> Debug.todo "isErr result ⇒ Display modal for error"

        RemoveEntry uid ->
            ({ model | entries = Dict.remove uid model.entries }
            , Cmd.none)

        -- params:
        UpdateCoefMin n ->
            ({ model | coefMin = n }, Cmd.none)

        UpdateCoefMax n ->
            ({ model | coefMax = n }, Cmd.none)

        UpdateShipFactor n ->
            ({ model | shipFactor = n }, Cmd.none)

        ReloadPage ->
            (model, reloadAndSkipCache)

        -- Other
        Focus result ->
            case result of
                Ok  _     -> (model, Cmd.none)
                Err error -> Debug.todo "isErr result ⇒ Display modal for error"

-- More HTML
hMain : List (Html.Attribute msg) -> List (Html msg) -> Html msg
hMain attributes children =
    Html.node "main" [] [ Html.div attributes children ]

-- VIEW
mkUid : Model -> (String, Model)
mkUid m =
    let cUid     = m.currentUid
        uid      = "entry-" ++ (String.fromInt cUid) ++ ":"
        newModel = {m | currentUid = cUid + 1}
    in (uid, newModel)

qtrRow : List (LayoutGrid.Property m)
qtrRow = [ LayoutGrid.span1Phone
         , LayoutGrid.span2Tablet
         , LayoutGrid.span3Desktop
         ]
fullRow : List (LayoutGrid.Property m)
fullRow = [ LayoutGrid.span4Phone
          , LayoutGrid.span8Tablet
          , LayoutGrid.span12Desktop
          ]


fmtFloat : Float -> String
fmtFloat = format { frenchLocale | decimals = 3
                                 , decimalSeparator = "." }

viewFloat : Outstanding Float -> Html msg
viewFloat = text << Result.unwrap "error" fmtFloat

-- TODO: Refactor with Html.Keyed
-- see https://guide.elm-lang.org/optimization/keyed.html
-- see https://package.elm-lang.org/packages/elm/html/latest/Html-Keyed/
viewEntry : Model -> (Int, Entry) -> Html Msg
viewEntry m (uid, e) =
    let rowId = "entry-" ++ (String.fromInt uid)
    in
    LayoutGrid.inner [ Options.id rowId ]
        [ LayoutGrid.cell qtrRow
              [ Html.p [] [viewFloat e.quantity] ]
        , LayoutGrid.cell qtrRow
              [ Html.p [] [ viewFloat e.price ] ]
        , LayoutGrid.cell qtrRow
              [ Html.label [] [
                     FormField.view []
                         [ Checkbox.view Mdc (rowId ++ ":is-shipped") m.mdc
                               [ Checkbox.checked e.isShipped
                               , Options.onClick <| ToggleEntryIsShipped uid
                               ]
                               [ ]
                         , Icon.view [ ] "local_shipping"
                         ] ] ]
        , LayoutGrid.cell qtrRow
            [ Html.label [] [
                   Button.view Mdc (rowId ++ ":delete-entry") m.mdc
                       [
                        Button.outlined
                       , Button.dense
                       , Button.ripple
                       , Options.onClick <| RemoveEntry uid
                       ]
                       [ Icon.view [] "delete" ] ] ]
        ]


view : Model -> Html Msg
view model =
    let theTotalQuantity = viewFloat (totalQuantity model)
        thePriceMean     = viewFloat (priceMean model)
        thePriceMeanMin  = viewFloat (priceMeanMin model)
        thePriceMeanMax  = viewFloat (priceMeanMax model)
    in
    Html.div []
        [ Options.styled Html.header [
               Theme.background,
               Elevation.z4, Theme.textPrimaryOnDark ]
              [ Options.styled Html.div
                    [ Theme.primaryBg ]
                    [ LayoutGrid.view []
                          [ LayoutGrid.cell fullRow
                                [ LayoutGrid.inner [ Options.id "results-label" ]
                                      [ LayoutGrid.cell qtrRow
                                            [ Html.label [] [ text "Total Qté" ] ]
                                      , LayoutGrid.cell qtrRow
                                          [ Html.label [] [ text "Prix Moy" ] ]
                                      , LayoutGrid.cell qtrRow
                                          [ Html.label [] [ text "Moy Min" ] ]
                                      , LayoutGrid.cell qtrRow
                                          [ Html.label [] [ text "Moy Max" ] ]
                                      ]
                                , LayoutGrid.inner [ Options.id "results" ]
                                    [ LayoutGrid.cell qtrRow
                                          [ Html.p [ id "total-quantity" ] [ theTotalQuantity ] ]
                                    , LayoutGrid.cell qtrRow
                                        [ Html.p [ id "price-mean" ]     [ thePriceMean ] ]
                                    , LayoutGrid.cell qtrRow
                                        [ Html.p [ id "price-mean-min" ] [ thePriceMeanMin ] ]
                                    , LayoutGrid.cell qtrRow
                                        [ Html.p [ id "price-mean-max" ] [ thePriceMeanMax ] ]
                                    ]
                                ]
                          ]
                    ]
              , Options.styled Html.div
                    [ Theme.background ]
                    [ LayoutGrid.view [ Options.id "new-entry" ]
                          [ LayoutGrid.cell qtrRow
                                [ Textfield.view Mdc "new-entry:quantity" model.mdc
                                      [ Textfield.label "Qté..."
                                      -- , Textfield.type_ "number"
                                      , Textfield.pattern "-?[0-9]*(\\.[0-9]+)?"
                                      -- , Textfield.dense
                                      , Textfield.autofocus
                                      , Textfield.value
                                          (showOutstanding fmtFloat model.newEntry.quantity)
                                      , onOutstandingInputMdc UpdateNewEntryQuantity
                                      , onOutstandingChangeMdc (String.toFloat) UpdateNewEntryQuantity
                                      ] []
                                , Textfield.helperText
                                    [ Textfield.validationMsg ]
                                    [ text nanMessage ]
                                ]
                          , LayoutGrid.cell qtrRow
                              [ Textfield.view Mdc "new-entry:price" model.mdc
                                    [ Textfield.label "Prix..."
                                      -- , Textfield.type_ "number"
                                    , Textfield.pattern "-?[0-9]*(\\.[0-9]+)?"
                                    -- , Textfield.dense
                                    , Textfield.value (showOutstanding fmtFloat model.newEntry.price)
                                    , onOutstandingInputMdc UpdateNewEntryPrice
                                    , onOutstandingChangeMdc String.toFloat UpdateNewEntryPrice
                                    ] []
                              , Textfield.helperText
                                  [ Textfield.validationMsg ]
                                  [ text nanMessage ]
                              ]
                          , LayoutGrid.cell qtrRow
                              [ FormField.view []
                                    [ Checkbox.view Mdc "new-entry:is-shipped" model.mdc
                                          [ Checkbox.checked model.newEntry.isShipped
                                          , Options.onClick ToggleNewEntryIsShipped
                                          ]
                                          [ ]
                                    , Icon.view [ ] "local_shipping"
                                    ]
                              ]
                          , LayoutGrid.cell qtrRow
                              [ Button.view Mdc "new-entry:add" model.mdc
                                    [ Button.outlined
                                    , Button.dense
                                    , Options.onClick AddNewEntry
                                    ]
                                    [ Icon.view [] "add" ]
                              ]
                          ]
                    ]
              ]
        , Options.styled hMain
            [ Options.id "entries" ]
            [ LayoutGrid.view []
                  [ LayoutGrid.cell fullRow (
                      List.map (viewEntry model) <| Dict.toList <| model.entries)
                  ]
            ]

        , Options.styled Html.footer
            [ Options.id "params"
            , Theme.background
            , Elevation.z4 ]
            [ LayoutGrid.view []
                  [ LayoutGrid.cell qtrRow
                        [ Textfield.view Mdc "params:coef-min" model.mdc
                              [ Textfield.label "Coef min... "
                              -- , Textfield.type_ "number"
                              , Textfield.pattern "-?[0-9]*(\\.[0-9]+)?"
                              -- , Textfield.dense
                              , Textfield.value (showOutstanding fmtFloat model.coefMin)
                              , onOutstandingInputMdc UpdateCoefMin
                              , onOutstandingChangeMdc
                                    (String.toFloat |> withDefault defaultCoefMin) UpdateCoefMin
                              ] [  ]
                        , Textfield.helperText
                            [ Textfield.validationMsg ]
                            [ text nanMessage ]
                        ]
                  , LayoutGrid.cell qtrRow
                        [ Textfield.view Mdc "params:coef-max" model.mdc
                              [ Textfield.label "Coef max..."
                              -- , Textfield.type_ "number"
                              , Textfield.pattern "-?[0-9]*(\\.[0-9]+)?"
                              -- , Textfield.dense
                              , Textfield.value (showOutstanding fmtFloat model.coefMax)
                              , onOutstandingInputMdc UpdateCoefMax
                              , onOutstandingChangeMdc
                                    (String.toFloat |> withDefault defaultCoefMax) UpdateCoefMax
                              ] [  ]
                        , Textfield.helperText
                            [ Textfield.validationMsg ]
                            [ text nanMessage ]
                        ]
                  , LayoutGrid.cell qtrRow
                        [ Textfield.view Mdc "params:ship-factor" model.mdc
                              [ Textfield.leadingIcon "local_shipping"
                              -- , Textfield.type_ "number"
                              , Textfield.label "(%)"
                              , Textfield.pattern "-?[0-9]*(\\.[0-9]+)?"
                              -- , Textfield.dense
                              , Textfield.value (showOutstanding String.fromFloat model.shipFactor)
                              , onOutstandingInputMdc UpdateShipFactor
                              , onOutstandingChangeMdc
                                    (String.toFloat |> withDefault defaultShipFactor) UpdateShipFactor
                              ] [  ]
                        , Textfield.helperText
                            [ Textfield.validationMsg ]
                            [ text nanMessage ]
                        ]
                  , LayoutGrid.cell qtrRow
                        [ Button.view Mdc "refresh" model.mdc
                              [ Button.outlined
                              , Button.dense
                              , Options.onClick ReloadPage
                              ]
                              [ Icon.view [] "refresh" ]
                        ]
                  ]
            ]
        ]
