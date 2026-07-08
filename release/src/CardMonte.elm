-- CardMonte.elm
module CardMonte exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (Html, button, div, h2, p, text)
import Html.Attributes exposing (class, classList, disabled)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Process
import Random
import Task
import Time
import Types exposing (..)
import Utils exposing (randomShuffleList, randomShuffleType)

type alias Model =
    { state : MonteState
    , cards : List Card
    , shuffleRound : Int
    , currentShuffleType : ShuffleType
    }

init : ( Model, Cmd Msg )
init =
    ( { state = MonteIdle
      , cards = [ { id = CardA, isTarget = False }, { id = CardB, isTarget = True }, { id = CardC, isTarget = False } ]
      , shuffleRound = 0
      , currentShuffleType = NoShuffle
      }
    , Cmd.none
    )

type Msg
    = StartGame
    | TriggerShuffle
    | PerformShuffle
    | ApplyAnimation ShuffleType
    | ApplyShuffle (List Card)
    | GuessCard CardId

update : Msg -> Model -> ( Model, Cmd Msg, Int )
update msg model =
    case msg of
        StartGame ->
            ( { model
                | state = MonteShowing
                , shuffleRound = 0
                , currentShuffleType = NoShuffle
                , cards = [ { id = CardA, isTarget = False }, { id = CardB, isTarget = True }, { id = CardC, isTarget = False } ]
              }
            , Process.sleep 2200 |> Task.perform (\_ -> TriggerShuffle)
            , 0
            )

        TriggerShuffle ->
            ( { model | state = MonteShaking, shuffleRound = 0 }
            , Task.succeed () |> Task.perform (\_ -> PerformShuffle)
            , 0
            )

        PerformShuffle ->
            if model.shuffleRound >= 6 then
                ( { model | state = MonteGuessing, currentShuffleType = NoShuffle }
                , Random.generate ApplyShuffle (randomShuffleList model.cards)
                , 0
                )

            else
                ( { model | shuffleRound = model.shuffleRound + 1 }
                , Random.generate (\animation -> ApplyAnimation animation) randomShuffleType
                , 0
                )

        ApplyAnimation animation ->
            ( { model | currentShuffleType = animation }
            , Process.sleep 1100 |> Task.perform (\_ -> PerformShuffle)
            , 0
            )

        ApplyShuffle shuffledList ->
            ( { model | cards = shuffledList }, Cmd.none, 0 )

        GuessCard chosenId ->
            let
                isCorrect =
                    model.cards
                        |> List.filter (\c -> c.id == chosenId)
                        |> List.map (\c -> c.isTarget)
                        |> List.head
                        |> Maybe.withDefault False

                balanceChange =
                    if isCorrect then
                        200

                    else
                        -20
            in
            ( { model | state = MonteResult isCorrect }
            , Cmd.none
            , balanceChange
            )

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

view : Model -> Html Msg
view model =
    let
        statusText =
            case model.state of
                MonteIdle ->
                    "Merk dir die Lady (🂽)! Danach werden sie gemischt."

                MonteShowing ->
                    "MERK DIR DIE POSITION!"

                MonteShaking ->
                    "AUGEN AUF! Die Karten rotieren..."

                MonteGuessing ->
                    "Wo ist die Lady (🂽) versteckt? Wähle weise!"

                MonteResult True ->
                    "🏆 GENIAL! Du hast die Lady gefunden! +200€"

                MonteResult False ->
                    "💀 FALSCH! Du hast die Lady leider nicht gefunden. -20€"

        isGuessing =
            model.state == MonteGuessing

        isRevealed =
            case model.state of
                MonteShowing ->
                    True

                MonteResult _ ->
                    True

                _ ->
                    False

        animationClass =
            case model.currentShuffleType of
                NoShuffle ->
                    "anim-none"

                SwapLeftMiddle ->
                    "anim-swap-left-middle"

                SwapMiddleRight ->
                    "anim-swap-middle-right"

                SwapLeftRight ->
                    "anim-swap-left-right"

                RotateClockwise ->
                    "anim-rotate-clockwise"

        renderKeyedCard card =
            let
                cardLabel =
                    if isRevealed then
                        if card.isTarget then
                            "🂽"

                        else
                            "🂡"

                    else
                        "🂠"

                cardColorClass =
                    if isRevealed && card.isTarget then
                        "card-red"

                    else if isRevealed then
                        "card-black"

                    else
                        "card-back"

                uniqueKey =
                    case card.id of
                        CardA ->
                            "cardA"

                        CardB ->
                            "cardB"

                        CardC ->
                            "cardC"
            in
            ( uniqueKey
            , button
                [ classList
                    [ ( "monte-card-item", True )
                    , ( cardColorClass, True )
                    , ( "clickable-guess", isGuessing )
                    ]
                , onClick (GuessCard card.id)
                , disabled (not isGuessing)
                ]
                [ text cardLabel ]
            )
    in
    div []
        [ h2 [] [ text "🃏 Find the Lady" ]
        , div [ class "roulette-status" ] [ text statusText ]
        , Keyed.node "div" [ class "monte-table", class animationClass ]
            (List.map renderKeyedCard model.cards)
        , case model.state of
            MonteIdle ->
                button [ class "btn action-btn monte-start-btn", onClick StartGame ]
                    [ text "Karten aufdecken & Mischen (Kostenlos)" ]

            MonteResult _ ->
                button [ class "btn action-btn monte-reset-btn", onClick StartGame ]
                    [ text "Nächstes Spiel wagen" ]

            _ ->
                div [ class "result-message placeholder" ] []
        ]