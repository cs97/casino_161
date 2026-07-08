-- RussianRoulette.elm
module RussianRoulette exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (Html, button, div, h2, p, text)
import Html.Attributes exposing (class, classList, disabled, style)
import Html.Events exposing (onClick)
import Process
import Random
import Task
import Time
import Types exposing (..)

type alias Model =
    { state : RussianRouletteState
    , turn : RussianRouletteTurn
    , rotation : Int
    , bulletChamber : Int
    , currentShot : Int
    }

init : ( Model, Cmd Msg )
init =
    ( { state = RouletteIdle
      , turn = PlayerTurn
      , rotation = 180
      , bulletChamber = 3
      , currentShot = 1
      }
    -- , Random.generate SetupBullet (Random.int 1 6)
    , Cmd.none
    )

type Msg
    = SetupBullet Int
    | StartGame
    | PullTrigger
    | AnimationFinish
    | DealerAutoPlay

update : Msg -> Model -> ( Model, Cmd Msg, Int )
update msg model =
    case msg of
        SetupBullet chamber ->
            ( { model | bulletChamber = chamber }, Cmd.none, 0 )

        StartGame ->
            ( { model | state = RouletteIdle, turn = PlayerTurn, rotation = 180, currentShot = 1 }
            , Random.generate SetupBullet (Random.int 1 6)
            , 0
            )

        PullTrigger ->
            case model.state of
                RouletteIdle ->
                    ( { model | state = RouletteFiring }
                    , Process.sleep 800 |> Task.perform (\_ -> AnimationFinish)
                    , 0
                    )

                _ ->
                    ( model, Cmd.none, 0 )

        AnimationFinish ->
            let
                isDeadShot =
                    model.currentShot == model.bulletChamber
            in
            if isDeadShot then
                case model.turn of
                    PlayerTurn ->
                        ( { model | state = RouletteDead PlayerTurn }, Cmd.none, -1000 )

                    DealerTurn ->
                        ( { model | state = RouletteWon }, Cmd.none, 1000 )

            else
                case model.turn of
                    PlayerTurn ->
                        ( { model
                            | turn = DealerTurn
                            , rotation = 0
                            , state = RouletteIdle
                            , currentShot = model.currentShot + 1
                          }
                        , Process.sleep 1500 |> Task.perform (\_ -> DealerAutoPlay)
                        , 0
                        )

                    DealerTurn ->
                        ( { model
                            | turn = PlayerTurn
                            , rotation = 180
                            , state = RouletteIdle
                            , currentShot = model.currentShot + 1
                          }
                        , Cmd.none
                        , 0
                        )

        DealerAutoPlay ->
            if model.turn == DealerTurn && model.state == RouletteIdle then
                ( { model | state = RouletteFiring }
                , Process.sleep 800 |> Task.perform (\_ -> AnimationFinish)
                , 0
                )
            else
                ( model, Cmd.none, 0 )

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

view : Model -> Html Msg
view model =
    let
        isPlayer =
            model.turn == PlayerTurn

        isFiring =
            model.state == RouletteFiring

        statusMessage =
            case model.state of
                RouletteIdle ->
                    if isPlayer then
                        "DU BIST DRAN! Betätige den Abzug..."

                    else
                        "GEGNER IST DRAN! Er zielt..."

                RouletteFiring ->
                    "*Klick*..."

                RouletteDead PlayerTurn ->
                    "💥 BAMM! Du wurdest getroffen!"

                RouletteDead DealerTurn ->
                    "💥 BAMM! Der Gegner wurde getroffen!"

                RouletteWon ->
                    "🎉 DER GEGNER WURDE GETROFFEN!"

        showResetButton =
            case model.state of
                RouletteDead _ ->
                    True

                RouletteWon ->
                    True

                _ ->
                    False
    in
    div []
        [ h2 [] [ text "🔫 Russisch Roulette" ]
        , p [] [ text ("Schuss-Zähler: " ++ String.fromInt model.currentShot ++ " / 6") ]
        , div
            [ classList
                [ ( "roulette-status", True )
                , ( "status-player-active", isPlayer && model.state == RouletteIdle )
                ]
            ]
            [ text statusMessage ]
        , div [ class "roulette-stage" ]
            [ div
                [ classList [ ( "revolver-container", True ), ( "revolver-shooting", isFiring ) ]
                , style "transform" ("rotate(" ++ String.fromInt model.rotation ++ "deg)")
                ]
                [ div [ class "revolver-barrel" ] [ text "▲" ]
                , div [ class "revolver-body" ] [ text "🔫" ]
                ]
            ]
        , if showResetButton then
            button [ class "btn action-btn roulette-btn-reset", onClick StartGame ]
                [ text "Nochmal spielen (1000€)" ]

          else
            button
                [ class "btn action-btn roulette-btn-fire"
                , onClick PullTrigger
                , disabled (not isPlayer || isFiring)
                ]
                [ text
                    (if isPlayer then
                        "Trigger betätigen!"

                     else
                        "Warte auf Gegner..."
                    )
                ]
        , viewResult model.state
        ]

viewResult : RussianRouletteState -> Html Msg
viewResult state =
    case state of
        RouletteWon ->
            div [ class "result-message" ]
                [ h2 [ class "text-success" ] [ text "🎉 +1000€ Gewonnen!" ] ]

        RouletteDead PlayerTurn ->
            div [ class "result-message" ]
                [ h2 [ class "text-danger" ] [ text "😢 -1000€ Verloren." ] ]

        _ ->
            div [ class "result-message placeholder" ] []
