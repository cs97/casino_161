-- WheelOfFortune.elm
module WheelOfFortune exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (Html, button, div, h2, text)
import Html.Attributes exposing (class, classList, disabled)
import Html.Events exposing (onClick)
import Process
import Random
import Svg exposing (Svg, circle, g, path, polygon, svg, text_)
import Svg.Attributes exposing (cx, cy, d, fill, fontSize, r, stroke, strokeWidth, textAnchor, transform, viewBox, x, y)
import Task
import Time
import Types exposing (..)
import Utils exposing (wheelSectors)

-- MODEL

type alias Model =
    { state : WheelState
    , rotation : Float
    }

init : ( Model, Cmd Msg )
init =
    ( { state = WheelIdle
      , rotation = 0.0
      }
    , Cmd.none
    )

-- UPDATE

type Msg
    = StartSpin
    | CalculateResult Int
    | RevealResult WheelSector Float

update : Msg -> Model -> ( Model, Cmd Msg, Int )
update msg model =
    case msg of
        StartSpin ->
            if model.state == WheelSpinning then
                ( model, Cmd.none, 0 )

            else
                ( { model | state = WheelSpinning }
                , Random.generate CalculateResult (Random.int 0 7)
                , -20
                )

        CalculateResult targetSectorId ->
            let
                selectedSector =
                    wheelSectors
                        |> List.filter (\s -> s.id == targetSectorId)
                        |> List.head
                        |> Maybe.withDefault { id = 1, label = "NIETE", multiplier = 0.0, color = "#d9534f", textCol = "#fff" }

                sectorAngle =
                    45.0

                targetAngle =
                    270.0 - (toFloat targetSectorId * sectorAngle) - (sectorAngle / 2.0)

                baseSpin =
                    2160.0

                finalRotation =
                    model.rotation
                        + baseSpin
                        + (targetAngle
                            - (model.rotation
                                - (toFloat (Basics.floor model.rotation // 360) * 360.0)
                              )
                          )
            in
            ( { model | rotation = finalRotation }
            , Process.sleep 3000
                |> Task.perform (\_ -> RevealResult selectedSector finalRotation)
            , 0
            )

        RevealResult sector _ ->
            let
                payout =
                    Basics.round (20.0 * sector.multiplier)
            in
            ( { model | state = WheelResult sector }
            , Cmd.none
            , payout
            )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

-- VIEW

renderWheelSector : WheelSector -> Svg Msg
renderWheelSector sector =
    let
        startAngle =
            toFloat sector.id * 45.0

        endAngle =
            startAngle + 45.0

        rad angle =
            angle * pi / 180.0

        rRadius =
            130.0

        x1 =
            String.fromFloat (150.0 + rRadius * cos (rad startAngle))

        y1 =
            String.fromFloat (150.0 + rRadius * sin (rad startAngle))

        x2 =
            String.fromFloat (150.0 + rRadius * cos (rad endAngle))

        y2 =
            String.fromFloat (150.0 + rRadius * sin (rad endAngle))

        pathData =
            "M 150 150 L " ++ x1 ++ " " ++ y1 ++ " A 130 130 0 0 1 " ++ x2 ++ " " ++ y2 ++ " Z"

        textAngle =
            startAngle + 22.5
    in
    g []
        [ path [ d pathData, fill sector.color, stroke "#222222", strokeWidth "2" ] []
        , text_
            [ x "235"
            , y "154"
            , fill sector.textCol
            , fontSize "9"
            , textAnchor "end"
            , transform ("rotate(" ++ String.fromFloat textAngle ++ ", 150, 150)")
            , Svg.Attributes.style "font-weight: bold; font-family: sans-serif;"
            ]
            [ Svg.text sector.label ]
        ]

view : Model -> Html Msg
view model =
    let
        statusText =
            case model.state of
                WheelIdle ->
                    "Drehe das Rad für 20 € und gewinne fette Preise!"

                WheelSpinning ->
                    "Das Rad rotiert wild... Wo bleibt es stehen?!"

                WheelResult sector ->
                    if sector.multiplier == 0.0 then
                        "😢 Schade! Das war leider eine Niete."
                    else
                        "🎉 Glückwunsch! Multiplikator " ++ sector.label ++ " getroffen!"

        isSpinning =
            model.state == WheelSpinning
    in
    div [ class "wheel-game-wrapper" ]
        [ h2 [] [ text "🎡 Lucky SVG Wheel" ]
        , div [ class "roulette-status" ] [ text statusText ]

        -- DAS HOCHWERTIGE SVG GLÜCKSRAD
        , div [ class "wheel-stage" ]
            [ svg
                [ viewBox "0 0 300 300"
                , Svg.Attributes.width "320"
                , Svg.Attributes.height "320"
                ]
                [ -- 1. Der rotierende Teil (g = Group-Element)
                  g
                    [ transform ("rotate(" ++ String.fromFloat model.rotation ++ ", 150, 150)")
                    , Svg.Attributes.style "transition: transform 3s cubic-bezier(0.1, 0.8, 0.2, 1);"
                    ]
                    (List.map renderWheelSector wheelSectors)

                -- 2. Der statische Stopper/Pfeil oben (zeigt auf 270 Grad / Top Center)
                , polygon [ Svg.Attributes.points "150,22 140,2 160,2", fill "#ffffff", stroke "#000000", strokeWidth "2" ] []
                , circle [ cx "150", cy "150", r "12", fill "#ffffff", stroke "#333", strokeWidth "3" ] []
                ]
            ]
        , button
            [ class "btn action-btn wheel-spin-btn"
            , onClick StartSpin
            , disabled (isSpinning)
            ]
            [ text
                (if isSpinning then
                    "Glücksrad dreht..."
                 else
                    "Für 20€ DREHEN!"
                )
            ]
        ]