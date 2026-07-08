-- WheelOfFortune.elm
module WheelOfFortune exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (Html, button, div, h2, span, text)
import Html.Attributes exposing (class, classList, disabled, style)
import Html.Events exposing (onClick)
import Process
import Random
import Svg exposing (Svg, circle, g, path, polygon, svg, text_)
import Svg.Attributes exposing (cx, cy, d, fill, fontSize, r, stroke, strokeWidth, textAnchor, transform, viewBox, x, y)
import Task
import Time
import Types exposing (..)



-- MODEL


type alias Model =
    { state : WheelState
    , rotation : Float
    , sectorCount : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { state = WheelIdle
      , rotation = 0.0
      , sectorCount = 8
      }
    , Cmd.none
    )



-- LOCAL HELPERS FOR DYNAMIC SECTORS


getDynamicSectors : Int -> List WheelSector
getDynamicSectors count =
    case count of
        10 ->
            [ { id = 0, label = "JACKPOT", multiplier = 8.5, color = "#ffcc00", textCol = "#000" }
            , { id = 1, label = "NIETE", multiplier = 0.0, color = "#d9534f", textCol = "#fff" }
            , { id = 2, label = "2x GEWINN", multiplier = 2.0, color = "#5cb85c", textCol = "#fff" }
            , { id = 3, label = "NIETE", multiplier = 0.0, color = "#d9534f", textCol = "#fff" }
            , { id = 4, label = "3x GEWINN", multiplier = 3.0, color = "#f0ad4e", textCol = "#fff" }
            , { id = 5, label = "NIETE", multiplier = 0.0, color = "#d9534f", textCol = "#fff" }
            , { id = 6, label = "4x GEWINN", multiplier = 4.0, color = "#5bc0de", textCol = "#fff" }
            , { id = 7, label = "NIETE", multiplier = 0.0, color = "#d9534f", textCol = "#fff" }
            , { id = 8, label = "5x GEWINN", multiplier = 5.0, color = "#9b59b6", textCol = "#fff" }
            , { id = 9, label = "NIETE", multiplier = 0.0, color = "#d9534f", textCol = "#fff" }
            ]

        12 ->
            [ { id = 0, label = "MEGA JACKPOT", multiplier = 10.0, color = "#ffcc00", textCol = "#000" }
            , { id = 1, label = "NIETE", multiplier = 0.0, color = "#d9534f", textCol = "#fff" }
            , { id = 2, label = "2x GEWINN", multiplier = 2.0, color = "#5cb85c", textCol = "#fff" }
            , { id = 3, label = "NIETE", multiplier = 0.0, color = "#d9534f", textCol = "#fff" }
            , { id = 4, label = "3x GEWINN", multiplier = 3.0, color = "#f0ad4e", textCol = "#fff" }
            , { id = 5, label = "NIETE", multiplier = 0.0, color = "#d9534f", textCol = "#fff" }
            , { id = 6, label = "4x GEWINN", multiplier = 4.0, color = "#5bc0de", textCol = "#fff" }
            , { id = 7, label = "NIETE", multiplier = 0.0, color = "#d9534f", textCol = "#fff" }
            , { id = 8, label = "5x GEWINN", multiplier = 5.0, color = "#9b59b6", textCol = "#fff" }
            , { id = 9, label = "NIETE", multiplier = 0.0, color = "#d9534f", textCol = "#fff" }
            , { id = 10, label = "6x GEWINN", multiplier = 6.0, color = "#1abc9c", textCol = "#fff" }
            , { id = 11, label = "NIETE", multiplier = 0.0, color = "#d9534f", textCol = "#fff" }
            ]

        _ ->
            [ { id = 0, label = "JACKPOT", multiplier = 7.5, color = "#ffcc00", textCol = "#000" }
            , { id = 1, label = "NIETE", multiplier = 0.0, color = "#d9534f", textCol = "#fff" }
            , { id = 2, label = "2x GEWINN", multiplier = 2.0, color = "#5cb85c", textCol = "#fff" }
            , { id = 3, label = "NIETE", multiplier = 0.0, color = "#d9534f", textCol = "#fff" }
            , { id = 4, label = "3x GEWINN", multiplier = 3.0, color = "#f0ad4e", textCol = "#fff" }
            , { id = 5, label = "NIETE", multiplier = 0.0, color = "#d9534f", textCol = "#fff" }
            , { id = 6, label = "4x GEWINN", multiplier = 4.0, color = "#5bc0de", textCol = "#fff" }
            , { id = 7, label = "NIETE", multiplier = 0.0, color = "#d9534f", textCol = "#fff" }
            ]



-- UPDATE


type Msg
    = StartSpin
    | ChangeSectorCount Int
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
                , Random.generate CalculateResult (Random.int 0 (model.sectorCount - 1))
                , -20
                )

        ChangeSectorCount newCount ->
            if model.state == WheelSpinning then
                ( model, Cmd.none, 0 )

            else
                ( { model | sectorCount = newCount, state = WheelIdle, rotation = 0.0 }, Cmd.none, 0 )

        CalculateResult targetSectorId ->
            let
                sectors =
                    getDynamicSectors model.sectorCount

                selectedSector =
                    sectors
                        |> List.filter (\s -> s.id == targetSectorId)
                        |> List.head
                        |> Maybe.withDefault { id = 1, label = "NIETE", multiplier = 0.0, color = "#d9534f", textCol = "#fff" }

                sectorAngle =
                    360.0 / toFloat model.sectorCount

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


renderWheelSector : Int -> WheelSector -> Svg Msg
renderWheelSector totalCount sector =
    let
        sectorAngle =
            360.0 / toFloat totalCount

        startAngle =
            toFloat sector.id * sectorAngle

        endAngle =
            startAngle + sectorAngle

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

        -- Bei 10 oder 12 Feldern verkleinern wir die Schriftgröße etwas
        fSize =
            if totalCount > 8 then "7" else "9"

        pathData =
            "M 150 150 L " ++ x1 ++ " " ++ y1 ++ " A 130 130 0 0 1 " ++ x2 ++ " " ++ y2 ++ " Z"

        textAngle =
            startAngle + (sectorAngle / 2.0)
    in
    g []
        [ path [ d pathData, fill sector.color, stroke "#222222", strokeWidth "2" ] []
        , text_
            [ x "235"
            , y "153"
            , fill sector.textCol
            , fontSize fSize
            , textAnchor "end"
            , transform ("rotate(" ++ String.fromFloat textAngle ++ ", 150, 150)")
            , Svg.Attributes.style "font-weight: bold; font-family: sans-serif;"
            ]
            [ Svg.text sector.label ]
        ]


view : Model -> Html Msg
view model =
    let
        sectors =
            getDynamicSectors model.sectorCount

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

        -- DYNAMISCHES SWITCH-UI (Felder umschalten)
        , div [ style "margin" "15px 0", style "display" "flex", style "justify-content" "center", style "gap" "10px" ]
            [ span [ style "align-self" "center", style "font-weight" "bold", style "color" "#fff" ] [ text "Felder wechseln: " ]
            , button [ classList [ ( "btn", True ), ( "active", model.sectorCount == 8 ) ], onClick (ChangeSectorCount 8), disabled isSpinning ] [ text "8 Segmente" ]
            , button [ classList [ ( "btn", True ), ( "active", model.sectorCount == 10 ) ], onClick (ChangeSectorCount 10), disabled isSpinning ] [ text "10 Segmente" ]
            , button [ classList [ ( "btn", True ), ( "active", model.sectorCount == 12 ) ], onClick (ChangeSectorCount 12), disabled isSpinning ] [ text "12 Segmente" ]
            ]

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
                    (List.map (renderWheelSector model.sectorCount) sectors)

                -- 2. Der statische Stopper/Pfeil oben
                , polygon [ Svg.Attributes.points "150,22 140,2 160,2", fill "#ffffff", stroke "#000000", strokeWidth "2" ] []
                , circle [ cx "150", cy "150", r "12", fill "#ffffff", stroke "#333", strokeWidth "3" ] []
                ]
            ]
        , button
            [ class "btn action-btn wheel-spin-btn"
            , onClick StartSpin
            , disabled isSpinning
            ]
            [ text
                (if isSpinning then
                    "Glücksrad dreht..."

                 else
                    "Für 20€ DREHEN!"
                )
            ]
        ]