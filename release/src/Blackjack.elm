-- Blackjack.elm
module Blackjack exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (Html, button, div, h2, h3, span, text)
import Html.Attributes exposing (class, classList, disabled)
import Html.Events exposing (onClick)
import Random
import Time
import Types exposing (..)
import Utils exposing (bjCalculateScore, bjCardGenerator, bjCardValue)

type alias Model =
    { playerHand : List BjCard
    , dealerHand : List BjCard
    , state : BjGameState
    }

init : ( Model, Cmd Msg )
init =
    ( { playerHand = []
      , dealerHand = []
      , state = BjPlayerTurn
      }
    --, Random.generate InitialDraw (Random.pair bjCardGenerator bjCardGenerator)
    , Cmd.none
    )

type Msg
    = InitialDraw ( BjCard, BjCard )
    | Hit
    | PlayerDrew BjCard
    | Stand
    | DealerDrew BjCard
    | Restart

update : Msg -> Model -> ( Model, Cmd Msg, Int )
update msg model =
    case msg of
        InitialDraw ( pCard, dCard ) ->
            ( { model | playerHand = [ pCard ], dealerHand = [ dCard ] }
            , Cmd.none
            , -20
            )

        Hit ->
            if model.state == BjPlayerTurn then
                ( model, Random.generate PlayerDrew bjCardGenerator, 0 )

            else
                ( model, Cmd.none, 0 )

        PlayerDrew newCard ->
            let
                newHand =
                    newCard :: model.playerHand

                score =
                    bjCalculateScore newHand

                primeModel =
                    { model | playerHand = newHand }
            in
            if score > 21 then
                ( { primeModel | state = BjPlayerBusted }, Cmd.none, 0 )

            else
                ( primeModel, Cmd.none, 0 )

        Stand ->
            if model.state == BjPlayerTurn then
                updateDealer { model | state = BjDealerTurn }

            else
                ( model, Cmd.none, 0 )

        DealerDrew newCard ->
            let
                newHand =
                    newCard :: model.dealerHand

                primeModel =
                    { model | dealerHand = newHand }
            in
            updateDealer primeModel

        Restart ->
            if List.length model.playerHand == 0 then
                ( model, Random.generate InitialDraw (Random.pair bjCardGenerator bjCardGenerator), -20 )

            else
                ( { model | playerHand = [], dealerHand = [], state = BjPlayerTurn }
                , Random.generate InitialDraw (Random.pair bjCardGenerator bjCardGenerator)
                , -20
                )

updateDealer : Model -> ( Model, Cmd Msg, Int )
updateDealer model =
    let
        dealerScore =
            bjCalculateScore model.dealerHand

        playerScore =
            bjCalculateScore model.playerHand
    in
    if dealerScore < playerScore && dealerScore < 21 then
        ( model, Random.generate DealerDrew bjCardGenerator, 0 )

    else
        let
            finalState =
                if dealerScore > 21 then
                    BjDealerBusted

                else if playerScore > dealerScore then
                    BjPlayerWins

                else if dealerScore > playerScore then
                    BjDealerWins

                else
                    BjPush

            balanceChange =
                case finalState of
                    BjPlayerWins ->
                        50

                    BjDealerBusted ->
                        50

                    BjPush ->
                        20

                    _ ->
                        0
        in
        ( { model | state = finalState }, Cmd.none, balanceChange )

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

viewBjCard : BjCard -> Html Msg
viewBjCard card =
    let
        ( sym, isRed ) =
            case card of
                BjAce ->
                    ( "🂡", False )

                BjTwo ->
                    ( "🂢", False )

                BjThree ->
                    ( "🂣", False )

                BjFour ->
                    ( "🂤", False )

                BjFive ->
                    ( "🂥", False )

                BjSix ->
                    ( "🂦", False )

                BjSeven ->
                    ( "🂧", False )

                BjEight ->
                    ( "🂨", False )

                BjNine ->
                    ( "🂩", False )

                BjTen ->
                    ( "🂪", False )

                BjJack ->
                    ( "🂫", False )

                BjQueen ->
                    ( "🂭", True )

                BjKing ->
                    ( "🂮", True )
    in
    span [ classList [ ( "bj-card-render", True ), ( "bj-red", isRed ) ] ]
        [ text sym ]

viewBjStatus : BjGameState -> String
viewBjStatus state =
    case state of
        BjPlayerTurn ->
            "Du bist am Zug. Ziehst du noch eine Karte oder hältst du?"

        BjDealerTurn ->
            "Dealer zieht Karten..."

        BjPlayerBusted ->
            "Du hast dich überkauft (über 21)! -20€"

        BjDealerBusted ->
            "Dealer hat sich überkauft! Du gewinnst +50€!"

        BjPlayerWins ->
            "Glückwunsch! Du hast mehr Punkte und gewinnst +50€!"

        BjDealerWins ->
            "Der Dealer gewinnt. -20€"

        BjPush ->
            "Unentschieden! Du bekommst deinen Einsatz zurück (+20€)."

view : Model -> Html Msg
view model =
    div [ class "blackjack-container" ]
        [ h2 [] [ text "🃏 Blackjack (Casino Edition)" ]
        , div [ class "roulette-status" ] [ text (viewBjStatus model.state) ]
        , div [ class "bj-sector dealer-sector" ]
            [ h3 []
                [ text ("Dealer (Punkte: " ++ String.fromInt (bjCalculateScore model.dealerHand) ++ ")") ]
            , div [ class "bj-hand-display" ]
                (List.map viewBjCard (List.reverse model.dealerHand))
            ]
        , div [ class "bj-sector player-sector" ]
            [ h3 []
                [ text ("Spieler (Punkte: " ++ String.fromInt (bjCalculateScore model.playerHand) ++ ")") ]
            , div [ class "bj-hand-display" ]
                (List.map viewBjCard (List.reverse model.playerHand))
            ]
        , div [ class "bj-controls" ]
            [ button
                [ class "btn bj-btn hit-btn"
                , onClick Hit
                , disabled (model.state /= BjPlayerTurn || List.isEmpty model.playerHand)
                ]
                [ text "Karte ziehen (Hit)" ]
            , button
                [ class "btn bj-btn stand-btn"
                , onClick Stand
                , disabled (model.state /= BjPlayerTurn || List.isEmpty model.playerHand)
                ]
                [ text "Halten (Stand)" ]
            , button
                [ class "btn action-btn bj-restart-btn"
                , onClick Restart
                , disabled (model.state == BjPlayerTurn)
                ]
                [ text "Einsatz setzen (20€)" ]
            ]
        ]
