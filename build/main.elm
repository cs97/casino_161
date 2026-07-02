module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h2, h3, option, p, select, span, text)
import Html.Attributes exposing (class, classList, disabled, style, value)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import Process
import Random
import Svg exposing (Svg, circle, g, path, polygon, svg, text_)
import Svg.Attributes exposing (cx, cy, d, fill, fontSize, r, stroke, strokeWidth, textAnchor, transform, viewBox, x, y)
import Task
import Time



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


type Symbol
    = Cherry
    | Seven
    | Diamond
    | Lemon



-- BLACKJACK SPECIFIC TYPES


type BjGameState
    = BjPlayerTurn
    | BjDealerTurn
    | BjPlayerBusted
    | BjDealerBusted
    | BjPlayerWins
    | BjDealerWins
    | BjPush


type BjCard
    = BjAce
    | BjTwo
    | BjThree
    | BjFour
    | BjFive
    | BjSix
    | BjSeven
    | BjEight
    | BjNine
    | BjTen
    | BjJack
    | BjQueen
    | BjKing



-- GLÜCKSRAD TYPEN


type WheelState
    = WheelIdle
    | WheelSpinning
    | WheelResult WheelSector


type alias WheelSector =
    { id : Int
    , label : String
    , multiplier : Float
    , color : String
    , textCol : String
    }



-- Die 8 Sektoren des Glücksrads exakt nach mathematischer Anordnung definiert


wheelSectors : List WheelSector
wheelSectors =
    [ { id = 0, label = "JACKPOT", multiplier = 7.5, color = "#ffcc00", textCol = "#000" } -- 0° bis 45°
    , { id = 1, label = "NIETE", multiplier = 0.0, color = "#d9534f", textCol = "#fff" } -- 45° bis 90°
    , { id = 2, label = "2x GEWINN", multiplier = 2.0, color = "#5cb85c", textCol = "#fff" } -- 90° bis 135°
    , { id = 3, label = "NIETE", multiplier = 0.0, color = "#d9534f", textCol = "#fff" } -- 135° bis 180°
    , { id = 4, label = "3x GEWINN", multiplier = 3.0, color = "#f0ad4e", textCol = "#fff" } -- 180° bis 225°
    , { id = 5, label = "NIETE", multiplier = 0.0, color = "#d9534f", textCol = "#fff" } -- 225° bis 270°
    , { id = 6, label = "4x GEWINN", multiplier = 4.0, color = "#5bc0de", textCol = "#fff" } -- 270° bis 315°
    , { id = 7, label = "NIETE", multiplier = 0.0, color = "#d9534f", textCol = "#fff" } -- 315° bis 360°
    ]


type Page
    = Dashboard
    | CoinFlip
    | RussianRoulette
    | RockPaperScissors
    | CardMonte
    | SlotMachine
    | Blackjack
    | WheelOfFortune -- Ersetzt Spiel 7 Platzhalter
    | Leaderboard
    | Shop
    | GamePlaceholder Int


type alias Charm =
    { id : Int
    , name : String
    , multiplier : Float
    , price : Int
    , icon : String
    }


availableCharms : List Charm
availableCharms =
    [ { id = 1, name = "Kleeblatt", multiplier = 1.1, price = 300, icon = "🍀" }
    , { id = 2, name = "Hufeisen", multiplier = 1.3, price = 800, icon = "🐴" }
    , { id = 3, name = "Glückspilz", multiplier = 1.5, price = 1200, icon = "🍄" }
    , { id = 4, name = "Marienkäfer", multiplier = 1.8, price = 2000, icon = "🐞" }
    , { id = 5, name = "Hasenpfote", multiplier = 2.0, price = 2500, icon = "🐾" }
    ]


type alias Model =
    { currentPage : Page
    , balance : Int
    , dropdownValue : String

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

    -- SlotMachine
    , slot1 : Symbol
    , slot2 : Symbol
    , slot3 : Symbol
    , slotMessage : String
    , slotIsSpinning : Bool
    , slotSpinTicks : Int

    -- Blackjack
    , bjPlayerHand : List BjCard
    , bjDealerHand : List BjCard
    , bjState : BjGameState

    -- Glücksrad (Neu)
    , wheelState : WheelState
    , wheelRotation : Float

    -- Shop System Global Storage
    , ownedCharmIds : List Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { currentPage = Dashboard
      , balance = 100
      , dropdownValue = ""

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

      -- SlotMachine
      , slot1 = Cherry
      , slot2 = Cherry
      , slot3 = Cherry
      , slotMessage = "Drücke auf Drehen! (Kostet 10 €)"
      , slotIsSpinning = False
      , slotSpinTicks = 0

      -- Blackjack
      , bjPlayerHand = []
      , bjDealerHand = []
      , bjState = BjPlayerTurn

      -- Glücksrad
      , wheelState = WheelIdle
      , wheelRotation = 0.0

      -- Shop
      , ownedCharmIds = []
      }
    , Cmd.none
    )



-- HELPER FUNCTIONS FOR CHARMS / MULTIPLIERS


getActiveMultiplier : Model -> Float
getActiveMultiplier model =
    availableCharms
        |> List.filter (\charm -> List.member charm.id model.ownedCharmIds)
        |> List.map .multiplier
        |> List.maximum
        |> Maybe.withDefault 1.0


getActiveCharmName : Model -> String
getActiveCharmName model =
    availableCharms
        |> List.filter (\charm -> List.member charm.id model.ownedCharmIds)
        |> List.sortBy .multiplier
        |> List.reverse
        |> List.head
        |> Maybe.map .name
        |> Maybe.withDefault "Keiner"


getActiveCharmIcon : Model -> String
getActiveCharmIcon model =
    availableCharms
        |> List.filter (\charm -> List.member charm.id model.ownedCharmIds)
        |> List.sortBy .multiplier
        |> List.reverse
        |> List.head
        |> Maybe.map .icon
        |> Maybe.withDefault "🎲"



-- UPDATE


type Msg
    = NavigateTo Page
    | SelectDropdown String
      -- CoinFlip
    | SelectCoinSide Side
    | StartCoinSpin
    | CalculateCoinFlipResult Int
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
      -- SlotMachine
    | StartSlotSpin
    | SlotTick Time.Posix
    | SlotNewSlots ( Symbol, Symbol, Symbol )
      -- Blackjack
    | BjInitialDraw ( BjCard, BjCard )
    | BjHit
    | BjPlayerDrewCard BjCard
    | BjStand
    | BjDealerDrewCard BjCard
    | BjRestart
      -- Glücksrad (Neu)
    | StartWheelSpin
    | CalculateWheelResult Int
    | RevealWheelResult WheelSector Float
      -- Shop Interaction
    | BuyCharm Charm


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavigateTo page ->
            let
                baseModel =
                    { model | dropdownValue = "" }
            in
            if page == RussianRoulette then
                ( { baseModel | currentPage = page, rouletteState = RouletteIdle, rouletteTurn = PlayerTurn, rouletteRotation = 180, currentShot = 1 }
                , Random.generate SetupRussianRouletteBullet (Random.int 1 6)
                )

            else if page == RockPaperScissors then
                ( { baseModel | currentPage = page, rpsState = RPSIdle, rpsPlayerChoice = None, rpsDealerChoice = None, rpsPlayerScore = 0, rpsDealerScore = 0 }, Cmd.none )

            else if page == CardMonte then
                ( { baseModel | currentPage = page, monteState = MonteIdle, shuffleRound = 0, currentShuffleType = NoShuffle, monteCards = [ { id = CardA, isTarget = False }, { id = CardB, isTarget = True }, { id = CardC, isTarget = False } ] }, Cmd.none )

            else if page == SlotMachine then
                ( { baseModel | currentPage = page, slotIsSpinning = False, slotSpinTicks = 0, slotMessage = "Drücke auf Drehen! (Kostet 10 €)" }, Cmd.none )

            else if page == Blackjack then
                ( { baseModel | currentPage = page, bjPlayerHand = [], bjDealerHand = [], bjState = BjPlayerTurn }, Cmd.none )

            else if page == WheelOfFortune then
                ( { baseModel | currentPage = page, wheelState = WheelIdle, wheelRotation = 0.0 }, Cmd.none )

            else
                ( { baseModel | currentPage = page }, Cmd.none )

        SelectDropdown val ->
            case val of
                "leaderboard" ->
                    ( { model | currentPage = Leaderboard, dropdownValue = "" }, Cmd.none )

                "shop" ->
                    ( { model | currentPage = Shop, dropdownValue = "" }, Cmd.none )

                _ ->
                    ( { model | dropdownValue = "" }, Cmd.none )

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
                    ( { model | coinGameState = Spinning }, Random.generate CalculateCoinFlipResult (Random.int 1 100) )

        CalculateCoinFlipResult diceRoll ->
            let
                multiplier =
                    getActiveMultiplier model

                winningThreshold =
                    Basics.min 100 (Basics.round (50.0 * multiplier))

                won =
                    diceRoll <= winningThreshold

                landedSide =
                    if won then
                        model.coinSelection

                    else if model.coinSelection == Head then
                        Tail

                    else
                        Head

                currentFullTurns =
                    model.coinRotationDegrees // 360

                targetExtra =
                    if landedSide == Head then
                        0

                    else
                        180

                newRotation =
                    (currentFullTurns * 360) + 1800 + targetExtra
            in
            ( { model | coinRotationDegrees = newRotation }
            , Process.sleep 2000 |> Task.perform (\_ -> RevealCoinResult { won = won, landedOn = landedSide })
            )

        RevealCoinResult resultData ->
            let
                newBalance =
                    if resultData.won then
                        model.balance + 10

                    else
                        model.balance - 10
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
            let
                multiplier =
                    getActiveMultiplier model

                isDeadShot =
                    model.currentShot == model.bulletChamber

                finalDeathHit =
                    if isDeadShot then
                        (1.0 / multiplier) >= 1.0

                    else
                        False
            in
            if isDeadShot && finalDeathHit then
                case model.rouletteTurn of
                    PlayerTurn ->
                        ( { model | rouletteState = RouletteDead PlayerTurn, balance = model.balance - 1000 }, Cmd.none )

                    DealerTurn ->
                        ( { model | rouletteState = RouletteWon, balance = model.balance + 1000 }, Cmd.none )

            else if isDeadShot && not finalDeathHit && model.rouletteTurn == PlayerTurn then
                ( { model | rouletteTurn = DealerTurn, rouletteRotation = 0, rouletteState = RouletteIdle, currentShot = model.currentShot + 1 }
                , Process.sleep 1500 |> Task.perform (\_ -> RussianRouletteDealerAutoPlay)
                )

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

        GenerateDealerChoice generatedChoice ->
            let
                multiplier =
                    getActiveMultiplier model

                pChoice =
                    model.rpsPlayerChoice

                dChoice =
                    if multiplier > 1.0 && (pChoice == Rock && generatedChoice == Paper) then
                        Scissors

                    else if multiplier > 1.0 && (pChoice == Paper && generatedChoice == Scissors) then
                        Rock

                    else if multiplier > 1.0 && (pChoice == Scissors && generatedChoice == Rock) then
                        Paper

                    else
                        generatedChoice

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
                            model.balance + 20

                        RPSGameOver False ->
                            model.balance - 20

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

        -- SLOT MACHINE
        StartSlotSpin ->
            if model.slotIsSpinning then
                ( model, Cmd.none )

            else if model.balance < 10 then
                ( { model | slotMessage = "Nicht genug Geld! Geh zurück zum Dashboard." }, Cmd.none )

            else
                ( { model
                    | balance = model.balance - 10
                    , slotMessage = "Die Walzen laufen..."
                    , slotIsSpinning = True
                    , slotSpinTicks = 0
                  }
                , Cmd.none
                )

        SlotTick _ ->
            if model.slotSpinTicks >= 10 then
                ( model, Random.generate SlotNewSlots slotsGenerator )

            else
                ( { model | slotSpinTicks = model.slotSpinTicks + 1 }, Random.generate SlotNewSlots slotsGenerator )

        SlotNewSlots ( s1, s2, s3 ) ->
            if model.slotIsSpinning && model.slotSpinTicks < 10 then
                ( { model | slot1 = s1, slot2 = s2, slot3 = s3 }, Cmd.none )

            else
                let
                    multiplier =
                        getActiveMultiplier model

                    ( finalS1, finalS2, finalS3 ) =
                        if multiplier > 1.0 && s1 /= s2 && s2 /= s3 && s1 /= s3 then
                            ( s1, s1, s3 )

                        else
                            ( s1, s2, s3 )

                    ( winAmount, msgText ) =
                        if finalS1 == finalS2 && finalS2 == finalS3 then
                            case finalS1 of
                                Seven ->
                                    ( 100, "JACKPOT! 3 Siebenen! +100 €!" )

                                Diamond ->
                                    ( 60, "Wow! 3 Diamanten! +60 €!" )

                                Cherry ->
                                    ( 40, "Süß! 3 Kirschen! +40 €!" )

                                Lemon ->
                                    ( 30, "Sauer bringt Geld! 3 Zitronen! +30 €!" )

                        else if finalS1 == finalS2 || finalS2 == finalS3 || finalS1 == finalS3 then
                            ( 15, "Paar! +15 €." )

                        else
                            ( 0, "Leider verloren. Versuch es noch einmal!" )
                in
                ( { model
                    | slot1 = finalS1
                    , slot2 = finalS2
                    , slot3 = finalS3
                    , balance = model.balance + winAmount
                    , slotMessage = msgText
                    , slotIsSpinning = False
                  }
                , Cmd.none
                )

        -- BLACKJACK INTERACTION
        BjInitialDraw ( pCard, dCard ) ->
            ( { model | bjPlayerHand = [ pCard ], bjDealerHand = [ dCard ] }, Cmd.none )

        BjHit ->
            if model.bjState == BjPlayerTurn then
                ( model, Random.generate BjPlayerDrewCard bjCardGenerator )

            else
                ( model, Cmd.none )

        BjPlayerDrewCard newCard ->
            let
                multiplier =
                    getActiveMultiplier model

                currentScoreWithoutNewCard =
                    bjCalculateScore model.bjPlayerHand

                mitigatedCard =
                    if multiplier > 1.2 && (currentScoreWithoutNewCard + bjCardValue newCard) > 21 then
                        BjAce

                    else
                        newCard

                newHand =
                    mitigatedCard :: model.bjPlayerHand

                score =
                    bjCalculateScore newHand

                primeModel =
                    { model | bjPlayerHand = newHand }
            in
            if score > 21 then
                ( { primeModel | bjState = BjPlayerBusted }, Cmd.none )

            else
                ( primeModel, Cmd.none )

        BjStand ->
            if model.bjState == BjPlayerTurn then
                updateDealer { model | bjState = BjDealerTurn }

            else
                ( model, Cmd.none )

        BjDealerDrewCard newCard ->
            let
                newHand =
                    newCard :: model.bjDealerHand

                primeModel =
                    { model | bjDealerHand = newHand }
            in
            updateDealer primeModel

        BjRestart ->
            if model.balance < 20 then
                ( { model | currentPage = Dashboard }, Cmd.none )

            else
                ( { model | bjPlayerHand = [], bjDealerHand = [], bjState = BjPlayerTurn, balance = model.balance - 20 }
                , Random.generate BjInitialDraw (Random.pair bjCardGenerator bjCardGenerator)
                )

        -- GLÜCKSRAD LOGIK (NEU)
        StartWheelSpin ->
            if model.wheelState == WheelSpinning then
                ( model, Cmd.none )

            else if model.balance < 20 then
                ( model, Cmd.none )
                -- Verhindert Drehen ohne Geld

            else
                -- 20€ sofort abziehen vor dem Drehen!
                ( { model
                    | balance = model.balance - 20
                    , wheelState = WheelSpinning
                  }
                , Random.generate CalculateWheelResult (Random.int 0 7)
                )

        CalculateWheelResult targetSectorId ->
            let
                -- Finde das ausgewählte Segment aus der Definitionsliste
                selectedSector =
                    wheelSectors
                        |> List.filter (\s -> s.id == targetSectorId)
                        |> List.head
                        |> Maybe.withDefault { id = 1, label = "NIETE", multiplier = 0.0, color = "#d9534f", textCol = "#fff" }

                -- Ein Segment ist genau 45° breit (360° / 8)
                sectorAngle = 45.0
                
                -- Der Stopper befindet sich oben bei 270°.
                -- Da SVG-Kreise bei 3 Uhr (0°) starten und im Uhrzeigersinn laufen,
                -- müssen wir berechnen, wie weit das Zielfeld gedreht werden muss, damit es oben landet.
                -- Formel für exakte visuelle Synchronität mit dem oberen Pfeil:
                targetAngle = 270.0 - (toFloat targetSectorId * sectorAngle) - (sectorAngle / 2.0)
                
                -- Wir addieren einen massiven Basisschwung (6 volle Umdrehungen = 2160°),
                -- damit sich das Rad immer rasant und kräftig dreht.
                baseSpin = 2160.0
                
                -- Der neue Gesamtwinkel berechnet sich aus dem aktuellen Stand + Schwung + Zielversatz
                finalRotation = model.wheelRotation + baseSpin + (targetAngle - (model.wheelRotation - (toFloat (Basics.floor model.wheelRotation // 360) * 360.0)))
            in
            ( { model | wheelRotation = finalRotation }
            , Process.sleep 3000 |> Task.perform (\_ -> RevealWheelResult selectedSector finalRotation)
            )

        RevealWheelResult sector finalAngle ->
            let
                -- Eventuelle Shop-Multiplikatoren einrechnen (falls erwünscht, sonst rein Basiswert)
                charmMult =
                    getActiveMultiplier model

                payout =
                    Basics.round (20.0 * sector.multiplier * charmMult)

                newBalance =
                    model.balance + payout
            in
            ( { model
                | wheelState = WheelResult sector
                , balance = newBalance

                -- Modulo 360 halten, damit nachfolgende Spins weich laufen
                , wheelRotation = finalAngle
              }
            , Cmd.none
            )

        -- SHOP KAUFLOGIK
        BuyCharm charm ->
            let
                alreadyOwned =
                    List.member charm.id model.ownedCharmIds

                canAfford =
                    model.balance >= charm.price
            in
            if canAfford && not alreadyOwned then
                ( { model
                    | balance = model.balance - charm.price
                    , ownedCharmIds = charm.id :: model.ownedCharmIds
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )


updateDealer : Model -> ( Model, Cmd Msg )
updateDealer model =
    let
        dealerScore =
            bjCalculateScore model.bjDealerHand

        playerScore =
            bjCalculateScore model.bjPlayerHand
    in
    if dealerScore < playerScore && dealerScore < 21 then
        ( model, Random.generate BjDealerDrewCard bjCardGenerator )

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

            payout =
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
        ( { model | bjState = finalState, balance = model.balance + payout }, Cmd.none )


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


symbolGenerator : Random.Generator Symbol
symbolGenerator =
    Random.uniform Cherry [ Seven, Diamond, Lemon ]


slotsGenerator : Random.Generator ( Symbol, Symbol, Symbol )
slotsGenerator =
    Random.map3 (\s1 s2 s3 -> ( s1, s2, s3 ))
        symbolGenerator
        symbolGenerator
        symbolGenerator


bjCardGenerator : Random.Generator BjCard
bjCardGenerator =
    Random.uniform BjAce
        [ BjTwo
        , BjThree
        , BjFour
        , BjFive
        , BjSix
        , BjSeven
        , BjEight
        , BjNine
        , BjTen
        , BjJack
        , BjQueen
        , BjKing
        ]


bjCalculateScore : List BjCard -> Int
bjCalculateScore cards =
    let
        initialSum =
            List.foldl (\c acc -> acc + bjCardValue c) 0 cards

        countAces =
            List.filter (\c -> c == BjAce) cards |> List.length

        adjustAces sum acesLeft =
            if sum > 21 && acesLeft > 0 then
                adjustAces (sum - 10) (acesLeft - 1)

            else
                sum
    in
    adjustAces initialSum countAces


bjCardValue : BjCard -> Int
bjCardValue card =
    case card of
        BjAce ->
            11

        BjTwo ->
            2

        BjThree ->
            3

        BjFour ->
            4

        BjFive ->
            5

        BjSix ->
            6

        BjSeven ->
            7

        BjEight ->
            8

        BjNine ->
            9

        BjTen ->
            10

        BjJack ->
            10

        BjQueen ->
            10

        BjKing ->
            10



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.currentPage == SlotMachine && model.slotIsSpinning then
        Time.every 100 SlotTick

    else
        Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "width" "100vw"
        , style "height" "100vh"
        , style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        , style "position" "relative"
        ]
        [ -- TOP BAR: Bleibt absolut oben fixiert auf dem Holz
          div [ class "top-bar" ]
            [ button [ class "nav-home-btn", onClick (NavigateTo Dashboard) ] [ text "Home" ]
            , div [ class "top-right-controls" ]
                [ div [ class "charm-indicator" ] [ text (getActiveCharmIcon model ++ " " ++ getActiveCharmName model ++ " (" ++ String.fromFloat (getActiveMultiplier model) ++ "x)") ]
                , div
                    [ classList
                        [ ( "balance-display", True )
                        , ( "balance-positive", model.balance >= 0 )
                        , ( "balance-negative", model.balance < 0 )
                        ]
                    ]
                    [ text (String.fromInt model.balance ++ " €") ]
                , select [ class "nav-dropdown", onInput SelectDropdown, value model.dropdownValue ]
                    [ option [ value "" ] [ text "Menü" ]
                    , option [ value "leaderboard" ] [ text "Bestenliste" ]
                    , option [ value "shop" ] [ text "\u{1F6D2} Shop" ]
                    ]
                ]
            ]

        -- GAME CONTAINER: Roter Samt/Filz für die Spiele
        , div
            [ classList
                [ ( "game-container", True )
                , ( "dashboard-active", model.currentPage == Dashboard )
                ]
            ]
            [ case model.currentPage of
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

                SlotMachine ->
                    viewSlotMachine model

                Blackjack ->
                    viewBlackjack model

                WheelOfFortune ->
                    viewWheelOfFortune model

                Leaderboard ->
                    viewStaticPage "Bestenliste" "Hier entstehen bald die Highscores der reichsten Spieler!"

                Shop ->
                    viewShop model

                GamePlaceholder id ->
                    viewStaticPage ("Spiel " ++ String.fromInt id) "Dieses Spiel befindet sich aktuell noch in der Entwicklung!"
            ]
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
            , button [ class "game-card slot-card", onClick (NavigateTo SlotMachine) ] [ text "🎰 Einarmiger Bandit" ]
            , button [ class "game-card blackjack-card", onClick (NavigateTo Blackjack) ] [ text "🃏 Blackjack" ]
            , button [ class "game-card wheel-card", onClick (NavigateTo WheelOfFortune) ] [ text "🎡 Glücksrad (SVG)" ]
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
                            "🎉 +10€ Gewonnen!"

                         else
                            "😢 -10€ Verloren."
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
            div [ class "result-message" ]
                [ h2 [ class "text-success" ] [ text "🎉 +1000€ Gewonnen!" ] ]

        RouletteDead PlayerTurn ->
            div [ class "result-message" ]
                [ h2 [ class "text-danger" ] [ text "😢 -1000€ Verloren." ] ]

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
                    "🏆 MATCH-SIEG! Du gewinnst 20€!"

                RPSGameOver False ->
                    "💀 MATCH-NIEDERLAGE! Du verlierst 20€."
    in
    div []
        [ h2 [] [ text "✂️ Schere Stein Papier" ]
        , div [ class "rps-scoreboard" ]
            [ div [ class "score-box" ] [ p [] [ text "Du" ], h1 [] [ text (String.fromInt model.rpsPlayerScore) ] ]
            , div [ class "score-divider" ] [ text "VS" ]
            , div [ class "score-box" ] [ p [] [ text "Gegner" ], h1 [] [ text (String.fromInt model.rpsDealerScore) ] ]
            ]
        , div [ class "roulette-status" ] [ text statusText ]
        , div [ class "rps-arena" ]
            [ div [ class "rps-hand-wrapper" ]
                [ p [] [ text "Deine Hand" ]
                , div [ classList [ ( "rps-hand player-hand", True ), ( "hand-shake", isShaking ) ] ] [ text (toEmoji model.rpsPlayerChoice isShaking) ]
                ]
            , div [ class "rps-hand-wrapper" ]
                [ p [] [ text "Gegner" ]
                , div [ classList [ ( "rps-hand dealer-hand", True ), ( "hand-shake", isShaking ) ] ] [ text (toEmoji model.rpsDealerChoice isShaking) ]
                ]
            ]
        , if isGameOver then
            button [ class "btn action-btn rps-btn-reset", onClick StartRPSGame ] [ text "Neues Match starten (20€)" ]

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
                , onClick (PlayerGuessCard card.id)
                , disabled (not isGuessing)
                ]
                [ text cardLabel ]
            )
    in
    div []
        [ h2 [] [ text "🃏 Find the Lady" ]
        , div [ class "roulette-status" ] [ text statusText ]
        , Keyed.node "div" [ class "monte-table", class animationClass ] (List.map renderKeyedCard model.monteCards)
        , case model.monteState of
            MonteIdle ->
                button [ class "btn action-btn monte-start-btn", onClick StartMonteGame ] [ text "Karten aufdecken & Mischen (Kostenlos)" ]

            MonteResult _ ->
                button [ class "btn action-btn monte-reset-btn", onClick StartMonteGame ] [ text "Nächstes Spiel wagen" ]

            _ ->
                div [ class "result-message placeholder" ] []
        ]



-- VIEW: SLOT MACHINE


symbolToString : Symbol -> String
symbolToString symbol =
    case symbol of
        Cherry ->
            "🍒"

        Seven ->
            "7️⃣"

        Diamond ->
            "💎"

        Lemon ->
            "🍋"


viewSlotMachine : Model -> Html Msg
viewSlotMachine model =
    div []
        [ h2 [] [ text "🎰 Einarmiger Bandit" ]
        , div [ class "roulette-status" ] [ text model.slotMessage ]
        , div [ class "slot-arena" ]
            [ div [ classList [ ( "slot-reel", True ), ( "blur-animation", model.slotIsSpinning ) ] ] [ text (symbolToString model.slot1) ]
            , div [ classList [ ( "slot-reel", True ), ( "blur-animation", model.slotIsSpinning && model.slotSpinTicks > 3 ) ] ] [ text (symbolToString model.slot2) ]
            , div [ classList [ ( "slot-reel", True ), ( "blur-animation", model.slotIsSpinning && model.slotSpinTicks > 6 ) ] ] [ text (symbolToString model.slot3) ]
            ]
        , div []
            [ button [ onClick StartSlotSpin, class "btn action-btn", disabled model.slotIsSpinning ]
                [ text
                    (if model.slotIsSpinning then
                        "Walzen drehen..."

                     else
                        "DREHEN! (10€)"
                    )
                ]
            ]
        ]



-- VIEW: BLACKJACK


viewBlackjack : Model -> Html Msg
viewBlackjack model =
    div [ class "blackjack-container" ]
        [ h2 [] [ text "🃏 Blackjack (Casino Edition)" ]
        , div [ class "roulette-status" ] [ text (viewBjStatus model.bjState) ]
        , div [ class "bj-sector dealer-sector" ]
            [ h3 [] [ text ("Dealer (Punkte: " ++ String.fromInt (bjCalculateScore model.bjDealerHand) ++ ")") ]
            , div [ class "bj-hand-display" ] (List.map viewBjCard (List.reverse model.bjDealerHand))
            ]
        , div [ class "bj-sector player-sector" ]
            [ h3 [] [ text ("Spieler (Punkte: " ++ String.fromInt (bjCalculateScore model.bjPlayerHand) ++ ")") ]
            , div [ class "bj-hand-display" ] (List.map viewBjCard (List.reverse model.bjPlayerHand))
            ]
        , div [ class "bj-controls" ]
            [ button [ class "btn bj-btn hit-btn", onClick BjHit, disabled (model.bjState /= BjPlayerTurn || List.isEmpty model.bjPlayerHand) ] [ text "Karte ziehen (Hit)" ]
            , button [ class "btn bj-btn stand-btn", onClick BjStand, disabled (model.bjState /= BjPlayerTurn || List.isEmpty model.bjPlayerHand) ] [ text "Halten (Stand)" ]
            , button [ class "btn action-btn bj-restart-btn", onClick BjRestart, disabled (model.bjState == BjPlayerTurn) ] [ text "Einsatz setzen (20€)" ]
            ]
        ]


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

                -- Schickes rotes Herz-Symbol-Mapping im CSS simuliert
                BjKing ->
                    ( "🂮", True )
    in
    span [ classList [ ( "bj-card-render", True ), ( "bj-red", isRed ) ] ] [ text sym ]



-- VIEW: GLÜCKSRAD (NEU MIT INTERAKTIVEM SVG)


viewWheelOfFortune : Model -> Html Msg
viewWheelOfFortune model =
    let
        statusText =
            case model.wheelState of
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
            model.wheelState == WheelSpinning
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
                    [ transform ("rotate(" ++ String.fromFloat model.wheelRotation ++ ", 150, 150)")
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
            , onClick StartWheelSpin
            , disabled (isSpinning || model.balance < 20)
            ]
            [ text
                (if isSpinning then
                    "Glücksrad dreht..."

                 else
                    "Für 20€ DREHEN!"
                )
            ]
        ]



-- Hilfsfunktion: Zeichnet genau ein Tortenstück (45 Grad) inklusive gedrehtem Text


renderWheelSector : WheelSector -> Svg Msg
renderWheelSector sector =
    let
        -- Jedes Feld hat eine Breite von 45 Grad
        startAngle =
            toFloat sector.id * 45.0

        endAngle =
            startAngle + 45.0

        -- Umwandlung von Grad in Bogenmaß für SVG-Kreisberechnung
        rad angle =
            angle * pi / 180.0

        -- Koordinaten für den äußeren Kreisbogen (Mittelpunkt 150,150, Radius 130)
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

        -- SVG Path Befehl für ein Tortenstück (Move to 150 150, Line to X1 Y1, Arc to X2 Y2, Close)
        pathData =
            "M 150 150 L " ++ x1 ++ " " ++ y1 ++ " A 130 130 0 0 1 " ++ x2 ++ " " ++ y2 ++ " Z"

        -- Textrotation genau in die Mitte des Tortenstücks platziert
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



-- VIEW: STATIC PAGES


viewStaticPage : String -> String -> Html Msg
viewStaticPage titel beschreibung =
    div [ class "static-page" ] [ h2 [] [ text titel ], p [] [ text beschreibung ] ]



-- VIEW: SHOP (LUCKY SHOP)


viewShop : Model -> Html Msg
viewShop model =
    div [ class "shop-container" ]
        [ h2 [ class "casino-title shop-main-title" ] [ text "Lucky Shop" ]
        , p [ class "casino-subtitle" ] [ text "Erwerbe permanente Glücksbringer. Nur der höchste Effekt schützt dich aktiv!" ]
        , div [ class "shop-grid" ]
            (List.map (viewShopItem model) availableCharms)
        ]


viewShopItem : Model -> Charm -> Html Msg
viewShopItem model charm =
    let
        isOwned =
            List.member charm.id model.ownedCharmIds

        canAfford =
            model.balance >= charm.price

        ( btnText, btnDisabled, btnClass ) =
            if isOwned then
                ( "Bereits im Besitz", True, "shop-btn owned" )

            else if not canAfford then
                ( "Zu wenig Geld (" ++ String.fromInt charm.price ++ "€)", True, "shop-btn locked" )

            else
                ( "Kaufen für " ++ String.fromInt charm.price ++ "€", False, "shop-btn" )
    in
    div [ class "shop-card" ]
        [ div [ class "shop-card-icon" ] [ text charm.icon ]
        , h3 [ class "shop-card-title" ] [ text charm.name ]
        , p [ class "shop-card-desc" ] [ text ("Erhöht deine Gewinnchancen global auf ein " ++ String.fromFloat charm.multiplier ++ "-faches!") ]
        , button [ class btnClass, onClick (BuyCharm charm), disabled btnDisabled ] [ text btnText ]
        ]
