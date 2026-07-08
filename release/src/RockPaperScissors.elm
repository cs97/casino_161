-- RockPaperScissors.elm
module RockPaperScissors exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (Html, button, div, h1, h2, p, text)
import Html.Attributes exposing (class, classList, disabled)
import Html.Events exposing (onClick)
import Process
import Random
import Task
import Time
import Types exposing (..)
import Utils exposing (randomRPS)

type alias Model =
    { state : RPSState
    , playerChoice : RPSChoice
    , dealerChoice : RPSChoice
    , playerScore : Int
    , dealerScore : Int
    }

init : ( Model, Cmd Msg )
init =
    ( { state = RPSIdle
      , playerChoice = None
      , dealerChoice = None
      , playerScore = 0
      , dealerScore = 0
      }
    , Cmd.none
    )

type Msg
    = StartGame
    | PlayerChoose RPSChoice
    | GenerateDealer RPSChoice
    | ResolveRound RPSChoice

update : Msg -> Model -> ( Model, Cmd Msg, Int )
update msg model =
    case msg of
        StartGame ->
            ( { model
                | state = RPSIdle
                , playerChoice = None
                , dealerChoice = None
                , playerScore = 0
                , dealerScore = 0
              }
            , Cmd.none
            , 0
            )

        PlayerChoose choice ->
            ( { model | state = RPSShaking, playerChoice = choice, dealerChoice = None }
            , Process.sleep 1200 |> Task.perform (\_ -> ResolveRound choice)
            , 0
            )

        ResolveRound pChoice ->
            ( model, Random.generate GenerateDealer randomRPS, 0 )

        GenerateDealer generatedChoice ->
            let
                dChoice =
                    generatedChoice

                roundRes =
                    if model.playerChoice == dChoice then
                        RoundTie

                    else if (model.playerChoice == Rock && dChoice == Scissors)
                        || (model.playerChoice == Paper && dChoice == Rock)
                        || (model.playerChoice == Scissors && dChoice == Paper) then
                        RoundPlayerWins

                    else
                        RoundDealerWins

                newPScore =
                    if roundRes == RoundPlayerWins then
                        model.playerScore + 1

                    else
                        model.playerScore

                newDScore =
                    if roundRes == RoundDealerWins then
                        model.dealerScore + 1

                    else
                        model.dealerScore

                nextState =
                    if newPScore >= 3 then
                        RPSGameOver True

                    else if newDScore >= 3 then
                        RPSGameOver False

                    else
                        RPSShowingRound roundRes

                balanceChange =
                    case nextState of
                        RPSGameOver True ->
                            20

                        RPSGameOver False ->
                            -20

                        _ ->
                            0
            in
            ( { model
                | state = nextState
                , dealerChoice = dChoice
                , playerScore = newPScore
                , dealerScore = newDScore
              }
            , Cmd.none
            , balanceChange
            )

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

view : Model -> Html Msg
view model =
    let
        isShaking =
            model.state == RPSShaking

        isGameOver =
            case model.state of
                RPSGameOver _ ->
                    True

                _ ->
                    False

        toEmoji choice shaking =
            if shaking then
                "✊"

            else
                case choice of
                    Rock ->
                        "✊"

                    Paper ->
                        "✋"

                    Scissors ->
                        "✌️"

                    None ->
                        "❓"

        statusText =
            case model.state of
                RPSIdle ->
                    "Wähle deine Hand! Wer zuerst 3 Punkte hat, gewinnt."

                RPSShaking ->
                    "Schere... Stein... Papier..."

                RPSShowingRound RoundTie ->
                    "Unentschieden in dieser Runde!"

                RPSShowingRound RoundPlayerWins ->
                    "Punkt für dich! 🎉"

                RPSShowingRound RoundDealerWins ->
                    "Punkt für den Gegner! \u{1F916}"

                RPSShowingRound RoundNone ->
                    ""

                RPSGameOver True ->
                    "🏆 MATCH-SIEG! Du gewinnst 20€!"

                RPSGameOver False ->
                    "💀 MATCH-NIEDERLAGE! Du verlierst 20€."
    in
    div []
        [ h2 [] [ text "✂️ Schere Stein Papier" ]
        , div [ class "rps-scoreboard" ]
            [ div [ class "score-box" ]
                [ p [] [ text "Du" ]
                , h1 [] [ text (String.fromInt model.playerScore) ]
                ]
            , div [ class "score-divider" ] [ text "VS" ]
            , div [ class "score-box" ]
                [ p [] [ text "Gegner" ]
                , h1 [] [ text (String.fromInt model.dealerScore) ]
                ]
            ]
        , div [ class "roulette-status" ] [ text statusText ]
        , div [ class "rps-arena" ]
            [ div [ class "rps-hand-wrapper" ]
                [ p [] [ text "Deine Hand" ]
                , div
                    [ classList
                        [ ( "rps-hand player-hand", True )
                        , ( "hand-shake", isShaking )
                        ]
                    ]
                    [ text (toEmoji model.playerChoice isShaking) ]
                ]
            , div [ class "rps-hand-wrapper" ]
                [ p [] [ text "Gegner" ]
                , div
                    [ classList
                        [ ( "rps-hand dealer-hand", True )
                        , ( "hand-shake", isShaking )
                        ]
                    ]
                    [ text (toEmoji model.dealerChoice isShaking) ]
                ]
            ]
        , if isGameOver then
            button [ class "btn action-btn rps-btn-reset", onClick StartGame ]
                [ text "Neues Match starten (20€)" ]

          else
            div [ class "rps-choices" ]
                [ button
                    [ class "btn rps-choice-btn"
                    , onClick (PlayerChoose Rock)
                    , disabled isShaking
                    ]
                    [ text "✊ Stein" ]
                , button
                    [ class "btn rps-choice-btn"
                    , onClick (PlayerChoose Paper)
                    , disabled isShaking
                    ]
                    [ text "✋ Papier" ]
                , button
                    [ class "btn rps-choice-btn"
                    , onClick (PlayerChoose Scissors)
                    , disabled isShaking
                    ]
                    [ text "✌️ Schere" ]
                ]
        ]