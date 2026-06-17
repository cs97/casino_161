module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h2, option, p, select, text)
import Html.Attributes exposing (class, classList, disabled, style, value)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import Process
import Random
import Task



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Side
    = Head
    | Tail


type GameState
    = Idle
    | Spinning
    | Result { won : Bool, landedOn : Side }


type RussianRouletteTurn
    = PlayerTurn
    | DealerTurn


type RussianRouletteState
    = RouletteIdle
    | RouletteFiring
    | RouletteDead RussianRouletteTurn
    | RouletteWon


type RPSChoice
    = Rock
    | Paper
    | Scissors
    | None


type RPSRoundResult
    = RoundTie
    | RoundPlayerWins
    | RoundDealerWins
    | RoundNone


type RPSState
    = RPSIdle
    | RPSShaking
    | RPSShowingRound RPSRoundResult
    | RPSGameOver Bool


type CardId
    = CardA
    | CardB
    | CardC


type alias Card =
    { id : CardId
    , isTarget : Bool
    }


type MonteState
    = MonteIdle
    | MonteShowing
    | MonteShaking
    | MonteGuessing
    | MonteResult Bool


type ShuffleType
    = NoShuffle
    | SwapLeftMiddle
    | SwapMiddleRight
    | SwapLeftRight
    | RotateClockwise


type Page
    = Dashboard
    | CoinFlip
    | RussianRoulette
    | RockPaperScissors
    | CardMonte
    | Leaderboard
    | Shop
    | GamePlaceholder Int


type alias Model =
    { currentPage : Page
    , balance : Int
    , dropdownOpen : Bool

    -- CoinFlip
    , coinSelection : Side
    , coinGameState : GameState
    , coinRotationDegrees : Int

    -- RussianRoulette
    , rouletteState : RussianRouletteState
    , rouletteTurn : RussianRouletteTurn
    , rouletteRotation : Int
    , bulletChamber : Int
    , currentShot : Int

    -- RockPaperScissors
    , rpsState : RPSState
    , rpsPlayerChoice : RPSChoice
    , rpsDealerChoice : RPSChoice
    , rpsPlayerScore : Int
    , rpsDealerScore : Int

    -- CardMonte
    , monteState : MonteState
    , monteCards : List Card
    , shuffleRound : Int
    , currentShuffleType : ShuffleType
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { currentPage = Dashboard
      , balance = 100
      , dropdownOpen = False

      -- CoinFlip
      , coinSelection = Head
      , coinGameState = Idle
      , coinRotationDegrees = 0

      -- RussianRoulette
      , rouletteState = RouletteIdle
      , rouletteTurn = PlayerTurn
      , rouletteRotation = 180
      , bulletChamber = 3
      , currentShot = 1

      -- RockPaperScissors
      , rpsState = RPSIdle
      , rpsPlayerChoice = None
      , rpsDealerChoice = None
      , rpsPlayerScore = 0
      , rpsDealerScore = 0

      -- CardMonte
      , monteState = MonteIdle
      , monteCards = [ { id = CardA, isTarget = False }, { id = CardB, isTarget = True }, { id = CardC, isTarget = False } ]
      , shuffleRound = 0
      , currentShuffleType = NoShuffle
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NavigateTo Page
    | SelectDropdown String
      -- CoinFlip
    | SelectCoinSide Side
    | StartCoinSpin
    | CalculateCoinFlipResult Side
    | RevealCoinResult { won : Bool, landedOn : Side }
      -- RussianRoulette
    | StartRussianRouletteGame
    | SetupRussianRouletteBullet Int
    | PullRussianRouletteTrigger
    | TriggerRussianRouletteAnimationFinish
    | RussianRouletteDealerAutoPlay
      -- RockPaperScissors
    | StartRPSGame
    | PlayerChooseRPS RPSChoice
    | GenerateDealerChoice RPSChoice
    | ResolveRPSRound RPSChoice
      -- CardMonte
    | StartMonteGame
    | TriggerShuffleStart
    | PerformShuffleStep
    | ApplyAnimationStep ShuffleType
    | ApplyShuffle (List Card)
    | PlayerGuessCard CardId


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavigateTo page ->
            if page == RussianRoulette then
                ( { model | currentPage = page, rouletteState = RouletteIdle, rouletteTurn = PlayerTurn, rouletteRotation = 180, currentShot = 1 }
                , Random.generate SetupRussianRouletteBullet (Random.int 1 6)
                )

            else if page == RockPaperScissors then
                ( { model | currentPage = page, rpsState = RPSIdle, rpsPlayerChoice = None, rpsDealerChoice = None, rpsPlayerScore = 0, rpsDealerScore = 0 }, Cmd.none )

            else if page == CardMonte then
                ( { model | currentPage = page, monteState = MonteIdle, shuffleRound = 0, currentShuffleType = NoShuffle, monteCards = [ { id = CardA, isTarget = False }, { id = CardB, isTarget = True }, { id = CardC, isTarget = False } ] }, Cmd.none )

            else
                ( { model | currentPage = page }, Cmd.none )

        SelectDropdown val ->
            case val of
                "leaderboard" ->
                    ( { model | currentPage = Leaderboard }, Cmd.none )

                "shop" ->
                    ( { model | currentPage = Shop }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        -- COINFLIP
        SelectCoinSide side ->
            case model.coinGameState of
                Spinning ->
                    ( model, Cmd.none )

                _ ->
                    ( { model | coinSelection = side, coinGameState = Idle }, Cmd.none )

        StartCoinSpin ->
            case model.coinGameState of
                Spinning ->
                    ( model, Cmd.none )

                _ ->
                    ( { model | coinGameState = Spinning }, Random.generate CalculateCoinFlipResult randomSide )

        CalculateCoinFlipResult side ->
            let
                currentFullTurns =
                    model.coinRotationDegrees // 360

                targetExtra =
                    if side == Head then
                        0

                    else
                        180

                newRotation =
                    (currentFullTurns * 360) + 1800 + targetExtra

                won =
                    model.coinSelection == side
            in
            ( { model | coinRotationDegrees = newRotation }
            , Process.sleep 2000 |> Task.perform (\_ -> RevealCoinResult { won = won, landedOn = side })
            )

        RevealCoinResult resultData ->
            let
                newBalance =
                    if resultData.won then
                        model.balance + 50

                    else
                        model.balance - 50
            in
            ( { model | coinGameState = Result resultData, balance = newBalance }, Cmd.none )

        -- RUSSIAN ROULETTE
        SetupRussianRouletteBullet chamber ->
            ( { model | bulletChamber = chamber }, Cmd.none )

        StartRussianRouletteGame ->
            ( { model | rouletteState = RouletteIdle, rouletteTurn = PlayerTurn, rouletteRotation = 180, currentShot = 1 }
            , Random.generate SetupRussianRouletteBullet (Random.int 1 6)
            )

        PullRussianRouletteTrigger ->
            case model.rouletteState of
                RouletteIdle ->
                    ( { model | rouletteState = RouletteFiring }
                    , Process.sleep 800 |> Task.perform (\_ -> TriggerRussianRouletteAnimationFinish)
                    )

                _ ->
                    ( model, Cmd.none )

        TriggerRussianRouletteAnimationFinish ->
            if model.currentShot == model.bulletChamber then
                case model.rouletteTurn of
                    PlayerTurn ->
                        ( { model | rouletteState = RouletteDead PlayerTurn, balance = model.balance - 1000 }, Cmd.none )

                    DealerTurn ->
                        ( { model | rouletteState = RouletteWon, balance = model.balance + 1000 }, Cmd.none )

            else
                case model.rouletteTurn of
                    PlayerTurn ->
                        ( { model | rouletteTurn = DealerTurn, rouletteRotation = 0, rouletteState = RouletteIdle, currentShot = model.currentShot + 1 }
                        , Process.sleep 1500 |> Task.perform (\_ -> RussianRouletteDealerAutoPlay)
                        )

                    DealerTurn ->
                        ( { model | rouletteTurn = PlayerTurn, rouletteRotation = 180, rouletteState = RouletteIdle, currentShot = model.currentShot + 1 }, Cmd.none )

        RussianRouletteDealerAutoPlay ->
            if model.rouletteTurn == DealerTurn && model.rouletteState == RouletteIdle then
                ( { model | rouletteState = RouletteFiring }
                , Process.sleep 800 |> Task.perform (\_ -> TriggerRussianRouletteAnimationFinish)
                )

            else
                ( model, Cmd.none )

        -- ROCK PAPER SCISSORS
        StartRPSGame ->
            ( { model | rpsState = RPSIdle, rpsPlayerChoice = None, rpsDealerChoice = None, rpsPlayerScore = 0, rpsDealerScore = 0 }, Cmd.none )

        PlayerChooseRPS choice ->
            ( { model | rpsState = RPSShaking, rpsPlayerChoice = choice, rpsDealerChoice = None }
            , Process.sleep 1200 |> Task.perform (\_ -> ResolveRPSRound choice)
            )

        ResolveRPSRound pChoice ->
            ( model, Random.generate GenerateDealerChoice randomRPS )

        GenerateDealerChoice dChoice ->
            let
                pChoice =
                    model.rpsPlayerChoice

                roundRes =
                    if pChoice == dChoice then
                        RoundTie

                    else if (pChoice == Rock && dChoice == Scissors) || (pChoice == Paper && dChoice == Rock) || (pChoice == Scissors && dChoice == Paper) then
                        RoundPlayerWins

                    else
                        RoundDealerWins

                newPScore =
                    if roundRes == RoundPlayerWins then
                        model.rpsPlayerScore + 1

                    else
                        model.rpsPlayerScore

                newDScore =
                    if roundRes == RoundDealerWins then
                        model.rpsDealerScore + 1

                    else
                        model.rpsDealerScore

                nextState =
                    if newPScore >= 3 then
                        RPSGameOver True

                    else if newDScore >= 3 then
                        RPSGameOver False

                    else
                        RPSShowingRound roundRes

                newBalance =
                    case nextState of
                        RPSGameOver True ->
                            model.balance + 200

                        RPSGameOver False ->
                            model.balance - 200

                        _ ->
                            model.balance
            in
            ( { model | rpsState = nextState, rpsDealerChoice = dChoice, rpsPlayerScore = newPScore, rpsDealerScore = newDScore, balance = newBalance }, Cmd.none )

        -- CARD MONTE
        StartMonteGame ->
            ( { model | monteState = MonteShowing, shuffleRound = 0, currentShuffleType = NoShuffle, monteCards = [ { id = CardA, isTarget = False }, { id = CardB, isTarget = True }, { id = CardC, isTarget = False } ] }
            , Process.sleep 2200 |> Task.perform (\_ -> TriggerShuffleStart)
            )

        TriggerShuffleStart ->
            ( { model | monteState = MonteShaking, shuffleRound = 0 }
            , Task.succeed () |> Task.perform (\_ -> PerformShuffleStep)
            )

        PerformShuffleStep ->
            if model.shuffleRound >= 6 then
                ( { model | monteState = MonteGuessing, currentShuffleType = NoShuffle }
                , Random.generate ApplyShuffle (randomShuffleList model.monteCards)
                )

            else
                ( { model | shuffleRound = model.shuffleRound + 1 }
                , Random.generate (\animation -> ApplyAnimationStep animation) randomShuffleType
                )

        ApplyAnimationStep animation ->
            ( { model | currentShuffleType = animation }
            , Process.sleep 1100 |> Task.perform (\_ -> PerformShuffleStep)
            )

        ApplyShuffle shuffledList ->
            ( { model | monteCards = shuffledList }, Cmd.none )

        PlayerGuessCard chosenId ->
            let
                isCorrect =
                    model.monteCards
                        |> List.filter (\c -> c.id == chosenId)
                        |> List.map (\c -> c.isTarget)
                        |> List.head
                        |> Maybe.withDefault False

                newBalance =
                    if isCorrect then
                        model.balance + 200

                    else
                        model.balance - 20
            in
            ( { model | monteState = MonteResult isCorrect, balance = newBalance }, Cmd.none )


randomSide : Random.Generator Side
randomSide =
    Random.uniform Head [ Tail ]


randomRPS : Random.Generator RPSChoice
randomRPS =
    Random.uniform Rock [ Paper, Scissors ]


randomShuffleType : Random.Generator ShuffleType
randomShuffleType =
    Random.uniform SwapLeftMiddle [ SwapMiddleRight, SwapLeftRight, RotateClockwise ]


randomShuffleList : List Card -> Random.Generator (List Card)
randomShuffleList cards =
    case cards of
        [ c1, c2, c3 ] ->
            Random.uniform [ c1, c2, c3 ]
                [ [ c2, c3, c1 ]
                , [ c3, c1, c2 ]
                , [ c2, c1, c3 ]
                , [ c1, c3, c2 ]
                , [ c3, c2, c1 ]
                ]

        _ ->
            Random.constant cards



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ classList
            [ ( "game-container", True )
            , ( "dashboard-active", model.currentPage == Dashboard )
            ]
        ]
        [ -- TOP BAR
          div [ class "top-bar" ]
            [ button [ class "nav-home-btn", onClick (NavigateTo Dashboard) ] [ text "Home" ]
            , div [ class "top-right-controls" ]
                [ div [ class "balance-display" ] [ text (String.fromInt model.balance ++ " €") ]
                , select [ class "nav-dropdown", onInput SelectDropdown ]
                    [ option [ value "" ] [ text "Menü" ]
                    , option [ value "leaderboard" ] [ text "Bestenliste" ]
                    , option [ value "shop" ] [ text "\u{1F6D2} Shop" ]
                    ]
                ]
            ]

        -- ROUTING
        , case model.currentPage of
            Dashboard ->
                viewDashboard

            CoinFlip ->
                viewCoinFlip model

            RussianRoulette ->
                viewRussianRoulette model

            RockPaperScissors ->
                viewRockPaperScissors model

            CardMonte ->
                viewCardMonte model

            Leaderboard ->
                viewStaticPage "Bestenliste" "Hier entstehen bald die Highscores der reichsten Spieler!"

            Shop ->
                viewStaticPage "\u{1F6D2} VIP Shop" "Hier kannst du bald virtuelle Goodies für deine Euro kaufen."

            GamePlaceholder id ->
                viewStaticPage ("Spiel " ++ String.fromInt id) "Dieses Spiel befindet sich aktuell noch in der Entwicklung!"
        ]



-- VIEW: DASHBOARD


viewDashboard : Html Msg
viewDashboard =
    div []
        [ h1 [ class "casino-title" ] [ text "CASINO 161" ]
        , p [ class "casino-subtitle" ] [ text "Wähle ein Spiel und fordere dein Glück heraus!" ]
        , div [ class "game-grid" ]
            [ button [ class "game-card coin-card", onClick (NavigateTo CoinFlip) ] [ text "\u{1FA99} Drehmünze" ]
            , button [ class "game-card roulette-card", onClick (NavigateTo RussianRoulette) ] [ text "🔫 Russisch Roulette" ]
            , button [ class "game-card rps-card", onClick (NavigateTo RockPaperScissors) ] [ text "✂️ Schere Stein Papier" ]
            , button [ class "game-card monte-card", onClick (NavigateTo CardMonte) ] [ text "🃏 Find the Lady" ]
            , button [ class "game-card", onClick (NavigateTo (GamePlaceholder 5)) ] [ text "🎱 Spiel 5" ]
            , button [ class "game-card", onClick (NavigateTo (GamePlaceholder 6)) ] [ text "🎡 Spiel 6" ]
            , button [ class "game-card", onClick (NavigateTo (GamePlaceholder 7)) ] [ text "💥 Spiel 7" ]
            , button [ class "game-card", onClick (NavigateTo (GamePlaceholder 8)) ] [ text "💎 Spiel 8" ]
            ]
        ]



-- VIEW: COINFLIP


viewCoinFlip : Model -> Html Msg
viewCoinFlip model =
    div []
        [ h2 [] [ text "\u{1FA99} Drehmünze" ]
        , div [ class "selection-zone" ]
            [ button [ classList [ ( "btn", True ), ( "active", model.coinSelection == Head ) ], onClick (SelectCoinSide Head), disabled (model.coinGameState == Spinning) ] [ text "Kopf" ]
            , button [ classList [ ( "btn", True ), ( "active", model.coinSelection == Tail ) ], onClick (SelectCoinSide Tail), disabled (model.coinGameState == Spinning) ] [ text "Zahl" ]
            ]
        , div [ class "coin-stage" ]
            [ div [ class "coin", style "transform" ("rotateY(" ++ String.fromInt model.coinRotationDegrees ++ "deg)") ]
                [ div [ class "coin-side front" ] [ text "👤" ]
                , div [ class "coin-side back" ] [ text "1" ]
                ]
            ]
        , button [ class "btn action-btn", onClick StartCoinSpin, disabled (model.coinGameState == Spinning) ]
            [ text
                (if model.coinGameState == Spinning then
                    "Münze fliegt..."

                 else
                    "Münze werfen!"
                )
            ]
        , viewCoinResult model.coinGameState
        ]


viewCoinResult : GameState -> Html Msg
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



-- VIEW: RUSSIAN ROULETTE


viewRussianRoulette : Model -> Html Msg
viewRussianRoulette model =
    let
        isPlayer =
            model.rouletteTurn == PlayerTurn

        isFiring =
            model.rouletteState == RouletteFiring

        statusMessage =
            case model.rouletteState of
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
            case model.rouletteState of
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
        , div [ classList [ ( "roulette-status", True ), ( "status-player-active", isPlayer && model.rouletteState == RouletteIdle ) ] ] [ text statusMessage ]
        , div [ class "roulette-stage" ]
            [ div [ classList [ ( "revolver-container", True ), ( "revolver-shooting", isFiring ) ], style "transform" ("rotate(" ++ String.fromInt model.rouletteRotation ++ "deg)") ]
                [ div [ class "revolver-barrel" ] [ text "▲" ]
                , div [ class "revolver-body" ] [ text "🔫" ]
                ]
            ]
        , if showResetButton then
            button [ class "btn action-btn roulette-btn-reset", onClick StartRussianRouletteGame ] [ text "Nochmal spielen (1000€)" ]

          else
            button [ class "btn action-btn roulette-btn-fire", onClick PullRussianRouletteTrigger, disabled (not isPlayer || isFiring) ]
                [ text
                    (if isPlayer then
                        "Trigger betätigen!"

                     else
                        "Warte auf Gegner..."
                    )
                ]
        , viewRouletteResult model.rouletteState
        ]


viewRouletteResult : RussianRouletteState -> Html Msg
viewRouletteResult state =
    case state of
        RouletteWon ->
            div [ class "result-message" ] [ h2 [ class "text-success" ] [ text "🎉 +1000€ Gewonnen!" ] ]

        RouletteDead PlayerTurn ->
            div [ class "result-message" ] [ h2 [ class "text-danger" ] [ text "😢 -1000€ Verloren." ] ]

        _ ->
            div [ class "result-message placeholder" ] []



-- VIEW: ROCK PAPER SCISSORS


viewRockPaperScissors : Model -> Html Msg
viewRockPaperScissors model =
    let
        isShaking =
            model.rpsState == RPSShaking

        isGameOver =
            case model.rpsState of
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
            case model.rpsState of
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
                    "🏆 MATCH-SIEG! Du gewinnst 200€!"

                RPSGameOver False ->
                    "💀 MATCH-NIEDERLAGE! Du verlierst 200€."
    in
    div []
        [ h2 [] [ text "✂️ Schere Stein Papier" ]

        -- Scoreboard
        , div [ class "rps-scoreboard" ]
            [ div [ class "score-box" ] [ p [] [ text "Du" ], h1 [] [ text (String.fromInt model.rpsPlayerScore) ] ]
            , div [ class "score-divider" ] [ text "VS" ]
            , div [ class "score-box" ] [ p [] [ text "Gegner" ], h1 [] [ text (String.fromInt model.rpsDealerScore) ] ]
            ]

        -- Status
        , div [ class "roulette-status" ] [ text statusText ]

        -- Arena
        , div [ class "rps-arena" ]
            [ div [ class "rps-hand-wrapper" ]
                [ p [] [ text "Deine Hand" ]
                , div [ classList [ ( "rps-hand player-hand", True ), ( "hand-shake", isShaking ) ] ]
                    [ text (toEmoji model.rpsPlayerChoice isShaking) ]
                ]
            , div [ class "rps-hand-wrapper" ]
                [ p [] [ text "Gegner" ]
                , div [ classList [ ( "rps-hand dealer-hand", True ), ( "hand-shake", isShaking ) ] ]
                    [ text (toEmoji model.rpsDealerChoice isShaking) ]
                ]
            ]

        -- Interaktion
        , if isGameOver then
            button [ class "btn action-btn rps-btn-reset", onClick StartRPSGame ] [ text "Neues Match starten (200€)" ]

          else
            div [ class "rps-choices" ]
                [ button [ class "btn rps-choice-btn", onClick (PlayerChooseRPS Rock), disabled isShaking ] [ text "✊ Stein" ]
                , button [ class "btn rps-choice-btn", onClick (PlayerChooseRPS Paper), disabled isShaking ] [ text "✋ Papier" ]
                , button [ class "btn rps-choice-btn", onClick (PlayerChooseRPS Scissors), disabled isShaking ] [ text "✌️ Schere" ]
                ]
        ]



-- VIEW: CARD MONTE (🃏 FIND THE LADY)


viewCardMonte : Model -> Html Msg
viewCardMonte model =
    let
        statusText =
            case model.monteState of
                MonteIdle ->
                    "Merk dir die Lady (🂽)! Danach werden sie gemischt."

                MonteShowing ->
                    "MERK DIR DIE POSITION!"

                MonteShaking ->
                    "AUGEN AUF! Die Karten rotieren..."

                MonteGuessing ->
                    "Wo ist die Lady (🂽) versteckt? Wähle weise!"

                MonteResult True ->
                    "🏆 GENIAL! Du hast die Lady gefunden! +20€"

                MonteResult False ->
                    "💀 FALSCH! Du hast die Lady leider nicht gefunden. -20€"

        isGuessing =
            model.monteState == MonteGuessing

        isRevealed =
            case model.monteState of
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
                            -- Die Herzdame (Lady)

                        else
                            "🂡"
                        -- Pik-Ass (statt Joker)

                    else
                        "🂠"

                -- Kartenrückseite
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
                , onClick (PlayerGuessCard card.id)
                , disabled (not isGuessing)
                ]
                [ text cardLabel ]
            )
    in
    div []
        [ h2 [] [ text "🃏 Find the Lady" ]
        , div [ class "roulette-status" ] [ text statusText ]
        , Keyed.node "div"
            [ class "monte-table", class animationClass ]
            (List.map renderKeyedCard model.monteCards)
        , case model.monteState of
            MonteIdle ->
                button [ class "btn action-btn monte-start-btn", onClick StartMonteGame ] [ text "Karten aufdecken & Mischen (Kostenlos)" ]

            MonteResult _ ->
                button [ class "btn action-btn monte-reset-btn", onClick StartMonteGame ] [ text "Nächstes Spiel wagen" ]

            _ ->
                div [ class "result-message placeholder" ] []
        ]


viewStaticPage : String -> String -> Html Msg
viewStaticPage titel beschreibung =
    div [ class "static-page" ] [ h2 [] [ text titel ], p [] [ text beschreibung ] ]
