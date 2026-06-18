module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h2, h3, option, p, select, span, text)
import Html.Attributes exposing (class, classList, disabled, style, value)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Process
import Random
import Task
import Time


-- GET / POST


apiUrl : String
apiUrl =
    "http://127.0.0.1:3000/score/spieler1"



-- 1. FUNKTION: Punkte abrufen (GET)


getScore : (Result Http.Error Int -> msg) -> Cmd msg
getScore toMsg =
    Http.get
        { url = apiUrl
        , expect = Http.expectJson toMsg (Decode.field "score" Decode.int)
        }



-- 2. FUNKTION: Punkte setzen (POST)


postScore : Int -> (Result Http.Error Int -> msg) -> Cmd msg
postScore neuerScore toMsg =
    Http.post
        { url = apiUrl
        , body = Http.jsonBody (Encode.object [ ( "score", Encode.int neuerScore ) ])
        , expect = Http.expectJson toMsg (Decode.field "score" Decode.int)
        }



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


type Page
    = Dashboard
    | CoinFlip
    | RussianRoulette
    | RockPaperScissors
    | CardMonte
    | SlotMachine
    | Blackjack
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
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { currentPage = Dashboard
      , balance = 100 -- Fallback-Wert, falls die API fehlschlägt
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
      }
    , getScore GotInitialScore -- Ruft den Score direkt beim Start ab
    )



-- UPDATE


type Msg
    = NavigateTo Page
    | SelectDropdown String
      -- API Initialisierung
    | GotInitialScore (Result Http.Error Int)
    | GotPostScoreResult (Result Http.Error Int) -- NEU: Für die Antwort des POST-Requests
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotInitialScore result ->
            case result of
                Ok initialScore ->
                    ( { model | balance = initialScore }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GotPostScoreResult result ->
            -- Hier fangen wir die API-Antwort nach dem Senden ab.
            -- Falls die API den neuen Stand validiert zurückgibt, könnte man ihn hier setzen.
            -- Im Fehlerfall machen wir aktuell nichts, um den Spielfluss nicht zu stören.
            case result of
                Ok aktuellerScoreVonApi ->
                    ( { model | balance = aktuellerScoreVonApi }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        NavigateTo page ->
            if page == RussianRoulette then
                ( { model | currentPage = page, rouletteState = RouletteIdle, rouletteTurn = PlayerTurn, rouletteRotation = 180, currentShot = 1 }
                , Random.generate SetupRussianRouletteBullet (Random.int 1 6)
                )

            else if page == RockPaperScissors then
                ( { model | currentPage = page, rpsState = RPSIdle, rpsPlayerChoice = None, rpsDealerChoice = None, rpsPlayerScore = 0, rpsDealerScore = 0 }, Cmd.none )

            else if page == CardMonte then
                ( { model | currentPage = page, monteState = MonteIdle, shuffleRound = 0, currentShuffleType = NoShuffle, monteCards = [ { id = CardA, isTarget = False }, { id = CardB, isTarget = True }, { id = CardC, isTarget = False } ] }, Cmd.none )

            else if page == SlotMachine then
                ( { model | currentPage = page, slotIsSpinning = False, slotSpinTicks = 0, slotMessage = "Drücke auf Drehen! (Kostet 10 €)" }, Cmd.none )

            else if page == Blackjack then
                ( { model | currentPage = page, bjPlayerHand = [], bjDealerHand = [], bjState = BjPlayerTurn }
                , Cmd.none
                )

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
                        model.balance + 10

                    else
                        model.balance - 10
            in
            ( { model | coinGameState = Result resultData, balance = newBalance }
            , postScore newBalance GotPostScoreResult -- AKTUALISIERT AN DIE API
            )

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
                        let
                            newBalance = model.balance - 1000
                        in
                        ( { model | rouletteState = RouletteDead PlayerTurn, balance = newBalance }
                        , postScore newBalance GotPostScoreResult -- AKTUALISIERT AN DIE API
                        )

                    DealerTurn ->
                        let
                            newBalance = model.balance + 1000
                        in
                        ( { model | rouletteState = RouletteWon, balance = newBalance }
                        , postScore newBalance GotPostScoreResult -- AKTUALISIERT AN DIE API
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
                            model.balance + 20

                        RPSGameOver False ->
                            model.balance - 20

                        _ ->
                            model.balance
            in
            ( { model | rpsState = nextState, rpsDealerChoice = dChoice, rpsPlayerScore = newPScore, rpsDealerScore = newDScore, balance = newBalance }
            , if newBalance /= model.balance then
                postScore newBalance GotPostScoreResult -- AKTUALISIERT AN DIE API (nur wenn sich die Balance geändert hat)
              else
                Cmd.none
            )

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
            ( { model | monteState = MonteResult isCorrect, balance = newBalance }
            , postScore newBalance GotPostScoreResult -- AKTUALISIERT AN DIE API
            )

        -- SLOT MACHINE
        StartSlotSpin ->
            if model.slotIsSpinning then
                ( model, Cmd.none )

            else if model.balance < 10 then
                ( { model | slotMessage = "Nicht genug Geld! Geh zurück zum Dashboard." }, Cmd.none )

            else
                let
                    newBalance = model.balance - 10
                in
                ( { model
                    | balance = newBalance
                    , slotMessage = "Die Walzen laufen..."
                    , slotIsSpinning = True
                    , slotSpinTicks = 0
                  }
                , postScore newBalance GotPostScoreResult -- AKTUALISIERT AN DIE API (Einsatz abgebucht)
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
                    ( winAmount, msgText ) =
                        if s1 == s2 && s2 == s3 then
                            case s1 of
                                Seven ->
                                    ( 100, "JACKPOT! 3 Siebenen! +100 €!" )

                                Diamond ->
                                    ( 60, "Wow! 3 Diamanten! +60 €!" )

                                Cherry ->
                                    ( 40, "Süß! 3 Kirschen! +40 €!" )

                                Lemon ->
                                    ( 30, "Sauer bringt Geld! 3 Zitronen! +30 €!" )

                        else if s1 == s2 || s2 == s3 || s1 == s3 then
                            ( 15, "Paar! +15 €." )

                        else
                            ( 0, "Leider verloren. Versuch es noch einmal!" )
                    
                    newBalance = model.balance + winAmount
                in
                ( { model
                    | slot1 = s1
                    , slot2 = s2
                    , slot3 = s3
                    , balance = newBalance
                    , slotMessage = msgText
                    , slotIsSpinning = False
                  }
                , if winAmount > 0 then 
                    postScore newBalance GotPostScoreResult -- AKTUALISIERT AN DIE API (Gewinn gutgeschrieben)
                  else 
                    Cmd.none
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
                newHand =
                    newCard :: model.bjPlayerHand

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
                let
                    newBalance = model.balance - 20
                in
                ( { model | bjPlayerHand = [], bjDealerHand = [], bjState = BjPlayerTurn, balance = newBalance }
                , Cmd.batch 
                    [ Random.generate BjInitialDraw (Random.pair bjCardGenerator bjCardGenerator)
                    , postScore newBalance GotPostScoreResult -- AKTUALISIERT AN DIE API (Einsatz abgebucht)
                    ]
                )


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
            
            newBalance = model.balance + payout
        in
        ( { model | bjState = finalState, balance = newBalance }
        , if payout > 0 then
            postScore newBalance GotPostScoreResult -- AKTUALISIERT AN DIE API (Gewinn/Push ausgezahlt)
          else
            Cmd.none
        )


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
                    , option [ value "shop" ] [ text "🛒 Shop" ]
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

            SlotMachine ->
                viewSlotMachine model

            Blackjack ->
                viewBlackjack model

            Leaderboard ->
                viewStaticPage "Bestenliste" "Hier entstehen bald die Highscores der reichsten Spieler!"

            Shop ->
                viewStaticPage "🛒 VIP Shop" "Hier kannst du bald virtuelle Goodies für deine Euro kaufen."

            GamePlaceholder id ->
                viewStaticPage ("Spiel " ++ String.fromInt id) "Dieses Spiel befindet sich aktuell noch in der Entwicklung!"
        ]


viewStaticPage : String -> String -> Html Msg
viewStaticPage title description =
    div [ class "static-page" ]
        [ h2 [] [ text title ]
        , p [] [ text description ]
        ]



-- VIEW: DASHBOARD


viewDashboard : Html Msg
viewDashboard =
    div []
        [ h1 [ class "casino-title" ] [ text "CASINO 161" ]
        , p [ class "casino-subtitle" ] [ text "Wähle ein Spiel und fordere dein Glück heraus!" ]
        , div [ class "game-grid" ]
            [ button [ class "game-card coin-card", onClick (NavigateTo CoinFlip) ] [ text "🪙 Drehmünze" ]
            , button [ class "game-card roulette-card", onClick (NavigateTo RussianRoulette) ] [ text "🔫 Russisch Roulette" ]
            , button [ class "game-card rps-card", onClick (NavigateTo RockPaperScissors) ] [ text "✂️ Schere Stein Papier" ]
            , button [ class "game-card monte-card", onClick (NavigateTo CardMonte) ] [ text "🃏 Find the Lady" ]
            , button [ class "game-card slot-card", onClick (NavigateTo SlotMachine) ] [ text "🎰 Einarmiger Bandit" ]
            , button [ class "game-card blackjack-card", onClick (NavigateTo Blackjack) ] [ text "🃏 Blackjack" ]
            , button [ class "game-card", onClick (NavigateTo (GamePlaceholder 7)) ] [ text "💥 Spiel 7" ]
            , button [ class "game-card", onClick (NavigateTo (GamePlaceholder 8)) ] [ text "💎 Spiel 8" ]
            ]
        ]



-- VIEW: COINFLIP


viewCoinFlip : Model -> Html Msg
viewCoinFlip model =
    div []
        [ h2 [] [ text "🪙 Drehmünze" ]
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
            div [ class "result-message" ] [ h2 [ class "text-success" ] [ text "🎉 +1000€ Gewonnen!" ] ]

        RouletteDead PlayerTurn ->
            div [ class "result-message" ] [ h2 [ class "text-danger" ] [ text "😢 -1000€ Verloren." ] ]

        _ ->
            div [ class "result-message placeholder" ] []

viewRockPaperScissors : Model -> Html Msg
viewRockPaperScissors model =
    div [] 
        [ h2 [] [ text "✂️ Schere Stein Papier" ]
        , p [] [ text "Hier kommt deine Schere-Stein-Papier Logik hin." ]
        ]


viewCardMonte : Model -> Html Msg
viewCardMonte model =
    div [] 
        [ h2 [] [ text "🃏 Find the Lady" ]
        , p [] [ text "Hier kommt die Card-Monte Logik hin." ]
        ]


viewSlotMachine : Model -> Html Msg
viewSlotMachine model =
    div [] 
        [ h2 [] [ text "🎰 Einarmiger Bandit" ]
        , p [] [ text ("Status: " ++ model.slotMessage) ]
        ]


viewBlackjack : Model -> Html Msg
viewBlackjack model =
    div [] 
        [ h2 [] [ text "🃏 Blackjack" ]
        , p [] [ text "Hier kommt deine Blackjack-Oberfläche hin." ]
        ]
