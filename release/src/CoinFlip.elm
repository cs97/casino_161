-- CoinFlip.elm
module CoinFlip exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (Html, button, div, h2, p, text)
import Html.Attributes exposing (class, classList, disabled, style)
import Html.Events exposing (onClick)
import Process
import Random
import Task
import Time
import Types exposing (..)

-- MODEL

type alias Model =
    { selection : Side
    , gameState : GameState
    , rotationDegrees : Int
    }

init : ( Model, Cmd Msg )
init =
    ( { selection = Head
      , gameState = Idle
      , rotationDegrees = 0
      }
    , Cmd.none
    )

-- UPDATE

type Msg
    = SelectSide Side
    | StartSpin
    | CalculateResult Int
    | RevealResult { won : Bool, landedOn : Side }

update : Msg -> Model -> ( Model, Cmd Msg, Int )
update msg model =
    case msg of
        SelectSide side ->
            case model.gameState of
                Spinning ->
                    ( model, Cmd.none, 0 )

                _ ->
                    ( { model | selection = side, gameState = Idle }, Cmd.none, 0 )

        StartSpin ->
            case model.gameState of
                Spinning ->
                    ( model, Cmd.none, 0 )

                _ ->
                    ( { model | gameState = Spinning }
                    , Random.generate CalculateResult (Random.int 1 100)
                    , 0
                    )

        CalculateResult diceRoll ->
            let
                -- 50% Gewinnchance
                won =
                    diceRoll <= 50

                -- Die gelandete Seite basierend auf Gewinn/Verlust
                landedSide =
                    if won then
                        model.selection
                    else if model.selection == Head then
                        Tail
                    else
                        Head

                -- Berechne die Rotation für die 3D-Animation
                currentFullTurns =
                    model.rotationDegrees // 360

                targetExtra =
                    if landedSide == Head then
                        0
                    else
                        180

                -- 5 volle Umdrehungen + Zielseite
                newRotation =
                    (currentFullTurns * 360) + 1800 + targetExtra
            in
            ( { model | rotationDegrees = newRotation }
            , Process.sleep 2000
                |> Task.perform (\_ -> RevealResult { won = won, landedOn = landedSide })
            , 0
            )

        RevealResult resultData ->
            let
                balanceChange =
                    if resultData.won then
                        10
                    else
                        -10
            in
            ( { model | gameState = Result resultData }
            , Cmd.none
            , balanceChange
            )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "\u{1FA99} Drehmünze" ]
        , viewSelection model
        , viewCoinStage model
        , viewControls model
        , viewResult model
        ]

viewSelection : Model -> Html Msg
viewSelection model =
    div [ class "selection-zone" ]
        [ button
            [ classList 
                [ ( "btn", True )
                , ( "active", model.selection == Head )
                ]
            , onClick (SelectSide Head)
            , disabled (model.gameState == Spinning)
            ]
            [ text "Kopf" ]
        , button
            [ classList 
                [ ( "btn", True )
                , ( "active", model.selection == Tail )
                ]
            , onClick (SelectSide Tail)
            , disabled (model.gameState == Spinning)
            ]
            [ text "Zahl" ]
        ]

viewCoinStage : Model -> Html Msg
viewCoinStage model =
    div [ class "coin-stage" ]
        [ div 
            [ class "coin"
            , style "transform" ("rotateY(" ++ String.fromInt model.rotationDegrees ++ "deg)")
            , style "transition" "transform 2s cubic-bezier(0.2, 0.8, 0.2, 1)"
            ]
            [ div [ class "coin-side front" ] 
                [ text "👤" ]  -- Kopf
            , div [ class "coin-side back" ] 
                [ text "1" ]   -- Zahl
            ]
        ]

viewControls : Model -> Html Msg
viewControls model =
    button
        [ class "btn action-btn"
        , onClick StartSpin
        , disabled (model.gameState == Spinning)
        ]
        [ text
            (if model.gameState == Spinning then
                "Münze fliegt..."
             else
                "Münze werfen!"
            )
        ]

viewResult : Model -> Html Msg
viewResult model =
    case model.gameState of
        Result { won, landedOn } ->
            let
                sideText =
                    if landedOn == Head then
                        "Kopf"
                    else
                        "Zahl"
            in
            div [ class "result-message" ]
                [ h2 [] [ text ("Es ist " ++ sideText ++ "!") ]
                , p 
                    [ class
                        (if won then
                            "text-success"
                         else
                            "text-danger"
                        )
                    ]
                    [ text
                        (if won then
                            "🎉 +10€ Gewonnen!"
                         else
                            "😢 -10€ Verloren."
                        )
                    ]
                ]

        _ ->
            div [ class "result-message placeholder" ] []