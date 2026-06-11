module Components.CoinFlip exposing (Side(..), GameState(..), view)

import Html exposing (Html, button, div, h2, p, text)
import Html.Attributes exposing (class, classList, disabled, style)
import Html.Events exposing (onClick)


type Side
    = Head
    | Tail


type GameState
    = Idle
    | Spinning
    | Result { won : Bool, landedOn : Side }


view : 
    { coinSelection : Side, coinGameState : GameState, coinRotationDegrees : Int }
    -> (Side -> msg)
    -> msg
    -> Html msg
view model onSelectCoinSide onStartCoinSpin =
    div []
        [ h2 [] [ text "🪙 Drehmünze" ]
        , div [ class "selection-zone" ]
            [ button
                [ classList [ ( "btn", True ), ( "active", model.coinSelection == Head ) ]
                , onClick (onSelectCoinSide Head)
                , disabled (model.coinGameState == Spinning)
                ]
                [ text "Kopf" ]
            , button
                [ classList [ ( "btn", True ), ( "active", model.coinSelection == Tail ) ]
                , onClick (onSelectCoinSide Tail)
                , disabled (model.coinGameState == Spinning)
                ]
                [ text "Zahl" ]
            ]
        , div [ class "coin-stage" ]
            [ div
                [ class "coin"
                , style "transform" ("rotateY(" ++ String.fromInt model.coinRotationDegrees ++ "deg)")
                ]
                [ div [ class "coin-side front" ] [ text "👤" ]
                , div [ class "coin-side back" ] [ text "1" ]
                ]
            ]
        , button
            [ class "btn action-btn"
            , onClick onStartCoinSpin
            , disabled (model.coinGameState == Spinning)
            ]
            [ text
                (if model.coinGameState == Spinning then
                    "Münze fliegt..."
                 else
                    "Münze werfen!"
                )
            ]
        , viewCoinResult model.coinGameState
        ]


viewCoinResult : GameState -> Html msg
viewCoinResult state =
    case state of
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
                            "🎉 +50€ Gewonnen!"
                         else
                            "😢 -50€ Verloren."
                        )
                    ]
                ]

        _ ->
            div [ class "result-message placeholder" ] []
