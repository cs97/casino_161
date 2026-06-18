module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Random



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type GameState
    = PlayerTurn
    | DealerTurn
    | PlayerBusted -- Über 21 Punkte
    | DealerBusted -- Dealer über 21 Punkte
    | PlayerWins
    | DealerWins
    | Push -- Unentschieden


type alias Model =
    { playerHand : List Card
    , dealerHand : List Card
    , state : GameState
    }



-- Wir starten das Spiel, indem wir direkt die ersten Karten ziehen lassen.
-- Der Einfachheit halber starten wir hier mit je einer Karte.


init : () -> ( Model, Cmd Msg )
init _ =
    ( { playerHand = []
      , dealerHand = []
      , state = PlayerTurn
      }
    , Random.generate InitialDraw (Random.pair cardGenerator cardGenerator)
    )


type Card
    = Ace
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King



-- UPDATE


type Msg
    = InitialDraw ( Card, Card )
    | Hit
    | PlayerDrewCard Card
    | Stand
    | DealerDrewCard Card
    | Restart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Initialisiert das Spiel mit den ersten beiden Karten
        InitialDraw ( pCard, dCard ) ->
            ( { model | playerHand = [ pCard ], dealerHand = [ dCard ] }
            , Cmd.none
            )

        -- Spieler verlangt eine Karte
        Hit ->
            if model.state == PlayerTurn then
                ( model, Random.generate PlayerDrewCard cardGenerator )

            else
                ( model, Cmd.none )

        PlayerDrewCard newCard ->
            let
                newHand =
                    newCard :: model.playerHand

                score =
                    calculateScore newHand

                primeModel =
                    { model | playerHand = newHand }
            in
            if score > 21 then
                ( { primeModel | state = PlayerBusted }, Cmd.none )

            else
                ( primeModel, Cmd.none )

        -- Spieler hält. Jetzt ist der Dealer am Zug.
        Stand ->
            if model.state == PlayerTurn then
                updateDealer { model | state = DealerTurn }

            else
                ( model, Cmd.none )

        DealerDrewCard newCard ->
            let
                newHand =
                    newCard :: model.dealerHand

                primeModel =
                    { model | dealerHand = newHand }
            in
            updateDealer primeModel

        -- Spiel zurücksetzen
        Restart ->
            init ()


updateDealer : Model -> ( Model, Cmd Msg )
updateDealer model =
    let
        dealerScore =
            calculateScore model.dealerHand

        playerScore =
            calculateScore model.playerHand
    in
    if dealerScore < playerScore && dealerScore < 21 then
        -- gemini hat 17 statt score gemacht
        ( model, Random.generate DealerDrewCard cardGenerator )

    else
        -- Dealer ist fertig, Ergebnis auswerten
        let
            finalState =
                if dealerScore > 21 then
                    DealerBusted

                else if playerScore > dealerScore then
                    PlayerWins

                else if dealerScore > playerScore then
                    DealerWins

                else
                    Push
        in
        ( { model | state = finalState }, Cmd.none )



-- Zufallsgenerator für Karten


cardGenerator : Random.Generator Card
cardGenerator =
    Random.uniform Ace
        [ Two
        , Three
        , Four
        , Five
        , Six
        , Seven
        , Eight
        , Nine
        , Ten
        , Jack
        , Queen
        , King
        ]



-- LOGIK: PUNKTE BERECHNEN
-- Ein Ass zählt als 11, es sei denn, man würde damit die 21 überschreiten. Then zählt es als 1.


calculateScore : List Card -> Int
calculateScore cards =
    let
        initialSum =
            List.foldl (\c acc -> acc + cardValue c) 0 cards

        countAces =
            List.filter (\c -> c == Ace) cards |> List.length

        -- Passt die Asse von 11 auf 1 an, wenn man sich überkauft hat
        adjustAces sum acesLeft =
            if sum > 21 && acesLeft > 0 then
                adjustAces (sum - 10) (acesLeft - 1)

            else
                sum
    in
    adjustAces initialSum countAces


cardValue : Card -> Int
cardValue card =
    case card of
        Ace ->
            11

        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6

        Seven ->
            7

        Eight ->
            8

        Nine ->
            9

        Ten ->
            10

        Jack ->
            10

        Queen ->
            10

        King ->
            10



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "font-family" "sans-serif", style "padding" "20px", style "max-width" "500px" ]
        [ h2 [] [ text "Blackjack (Elm Edition)" ]

        -- STATUSANZEIGE
        , div [ style "margin" "20px 0", style "font-weight" "bold", style "font-size" "1.2em" ]
            [ text (viewStatus model.state) ]

        -- DEALER BEREICH
        , div [ style "background" "#f0f0f0", style "padding" "10px", style "margin-bottom" "10px", style "border-radius" "5px" ]
            [ h3 [] [ text ("Dealer (Punkte: " ++ String.fromInt (calculateScore model.dealerHand) ++ ")") ]
            , div [ style "font-size" "4em" ] (List.map viewCard (List.reverse model.dealerHand))
            ]

        -- SPIELER BEREICH
        , div [ style "background" "#e0f7fa", style "padding" "10px", style "margin-bottom" "20px", style "border-radius" "5px" ]
            [ h3 [] [ text ("Spieler (Punkte: " ++ String.fromInt (calculateScore model.playerHand) ++ ")") ]
            , div [ style "font-size" "4em" ] (List.map viewCard (List.reverse model.playerHand))
            ]

        -- STEUERUNG BUTTONS
        , div []
            (if model.state == PlayerTurn then
                [ button ([ onClick Hit ] ++ btnStyle) [ text "Karte ziehen (Hit)" ]
                , button ([ onClick Stand, style "background" "#ff9800" ] ++ btnStyle) [ text "Halten (Stand)" ]
                ]

             else
                [ button ([ onClick Restart, style "background" "#4caf50" ] ++ btnStyle) [ text "Neues Spiel" ] ]
            )
        ]


viewStatus : GameState -> String
viewStatus state =
    case state of
        PlayerTurn ->
            "Du bist am Zug. Ziehen oder Halten?"

        DealerTurn ->
            "Dealer zieht Karten..."

        PlayerBusted ->
            "Du hast dich überkauft (über 21)! Dealer gewinnt."

        DealerBusted ->
            "Dealer hat sich überkauft! Du gewinnst!"

        PlayerWins ->
            "Glückwunsch! Du hast mehr Punkte und gewinnst!"

        DealerWins ->
            "Der Dealer gewinnt. Mehr Glück beim nächsten Mal!"

        Push ->
            "Unentschieden (Push)!"


viewCard : Card -> Html Msg
viewCard card =
    let
        sym =
            case card of
                Ace ->
                    "🂡"

                Two ->
                    "🂢"

                Three ->
                    "🂣"

                Four ->
                    "🂤"

                Five ->
                    "🂥"

                Six ->
                    "🂦"

                Seven ->
                    "🂧"

                Eight ->
                    "🂨"

                Nine ->
                    "🂩"

                Ten ->
                    "🂪"

                Jack ->
                    "🂫"

                Queen ->
                    "🂭"

                King ->
                    "🂮"
    in
    span [ style "margin-right" "5px" ] [ text sym ]


btnStyle : List (Attribute Msg)
btnStyle =
    [ style "padding" "10px 20px"
    , style "font-size" "1em"
    , style "margin-right" "10px"
    , style "cursor" "pointer"
    ]
