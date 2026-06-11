module Main exposing (main)

import Browser
import Components.CoinFlip as CoinFlip
import Components.Dashboard as Dashboard
import Components.RussianRoulette as Roulette
import Html exposing (Html, button, div, h2, option, p, select, text)
import Html.Attributes exposing (class, classList, value)
import Html.Events exposing (onClick, onInput)
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


type Page
    = Dashboard
    | CoinFlip
    | RussianRoulette
    | Leaderboard
    | Shop
    | GamePlaceholder Int


type alias Model =
    { currentPage : Page
    , balance : Int
    , dropdownOpen : Bool

    -- CoinFlip Zustand
    , coinSelection : CoinFlip.Side
    , coinGameState : CoinFlip.GameState
    , coinRotationDegrees : Int

    -- RussianRoulette Zustand
    , rouletteState : Roulette.RouletteState
    , rouletteTurn : Roulette.RouletteTurn
    , rouletteRotation : Int
    , bulletChamber : Int
    , currentShot : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { currentPage = Dashboard
      , balance = 100
      , dropdownOpen = False
      
      -- CoinFlip Init
      , coinSelection = CoinFlip.Head
      , coinGameState = CoinFlip.Idle
      , coinRotationDegrees = 0
      
      -- RussianRoulette Init
      , rouletteState = Roulette.RouletteIdle
      , rouletteTurn = Roulette.PlayerTurn
      , rouletteRotation = 180
      , bulletChamber = 3
      , currentShot = 1
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NavigateTo Page
    | SelectDropdown String
      -- CoinFlip Msgs:
    | SelectCoinSide CoinFlip.Side
    | StartCoinSpin
    | CalculateCoinFlipResult CoinFlip.Side
    | RevealCoinResult { won : Bool, landedOn : CoinFlip.Side }
      -- RussianRoulette Msgs:
    | StartRouletteGame
    | SetupRouletteBullet Int
    | PullRouletteTrigger
    | TriggerAnimationFinish
    | DealerAutoPlay


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavigateTo page ->
            if page == RussianRoulette then
                ( { model | currentPage = page, rouletteState = Roulette.RouletteIdle, rouletteTurn = Roulette.PlayerTurn, rouletteRotation = 180, currentShot = 1 }
                , Random.generate SetupRouletteBullet (Random.int 1 6)
                )
            else
                ( { model | currentPage = page }, Cmd.none )

        SelectDropdown val ->
            case val of
                "leaderboard" -> ( { model | currentPage = Leaderboard }, Cmd.none )
                "shop" -> ( { model | currentPage = Shop }, Cmd.none )
                _ -> ( model, Cmd.none )

        -- COIN FLIP UPDATE
        SelectCoinSide side ->
            case model.coinGameState of
                CoinFlip.Spinning -> ( model, Cmd.none )
                _ -> ( { model | coinSelection = side, coinGameState = CoinFlip.Idle }, Cmd.none )

        StartCoinSpin ->
            case model.coinGameState of
                CoinFlip.Spinning -> ( model, Cmd.none )
                _ -> ( { model | coinGameState = CoinFlip.Spinning }, Random.generate CalculateCoinFlipResult randomSide )

        CalculateCoinFlipResult side ->
            let
                currentFullTurns = model.coinRotationDegrees // 360
                targetExtra = if side == CoinFlip.Head then 0 else 180
                newRotation = (currentFullTurns * 360) + 1800 + targetExtra
                won = model.coinSelection == side
            in
            ( { model | coinRotationDegrees = newRotation }
            , Process.sleep 2000 |> Task.perform (\_ -> RevealCoinResult { won = won, landedOn = side })
            )

        RevealCoinResult resultData ->
            let
                newBalance = if resultData.won then model.balance + 50 else model.balance - 50
            in
            ( { model | coinGameState = CoinFlip.Result resultData, balance = newBalance }, Cmd.none )

        -- RUSSIAN ROULETTE UPDATE
        SetupRouletteBullet chamber ->
            ( { model | bulletChamber = chamber }, Cmd.none )

        StartRouletteGame ->
            ( { model | rouletteState = Roulette.RouletteIdle, rouletteTurn = Roulette.PlayerTurn, rouletteRotation = 180, currentShot = 1 }
            , Random.generate SetupRouletteBullet (Random.int 1 6)
            )

        PullRouletteTrigger ->
            case model.rouletteState of
                Roulette.RouletteIdle ->
                    ( { model | rouletteState = Roulette.RouletteFiring }
                    , Process.sleep 800 |> Task.perform (\_ -> TriggerAnimationFinish)
                    )
                _ ->
                    ( model, Cmd.none )

        TriggerAnimationFinish ->
            if model.currentShot == model.bulletChamber then
                case model.rouletteTurn of
                    Roulette.PlayerTurn ->
                        ( { model | rouletteState = Roulette.RouletteDead Roulette.PlayerTurn, balance = model.balance - 1000 }, Cmd.none )
                    Roulette.DealerTurn ->
                        ( { model | rouletteState = Roulette.RouletteWon, balance = model.balance + 1000 }, Cmd.none )
            else
                case model.rouletteTurn of
                    Roulette.PlayerTurn ->
                        ( { model | rouletteTurn = Roulette.DealerTurn, rouletteRotation = 0, rouletteState = Roulette.RouletteIdle, currentShot = model.currentShot + 1 }
                        , Process.sleep 1500 |> Task.perform (\_ -> DealerAutoPlay)
                        )
                    Roulette.DealerTurn ->
                        ( { model | rouletteTurn = Roulette.PlayerTurn, rouletteRotation = 180, rouletteState = Roulette.RouletteIdle, currentShot = model.currentShot + 1 }, Cmd.none )

        DealerAutoPlay ->
            if model.rouletteTurn == Roulette.DealerTurn && model.rouletteState == Roulette.RouletteIdle then
                ( { model | rouletteState = Roulette.RouletteFiring }
                , Process.sleep 800 |> Task.perform (\_ -> TriggerAnimationFinish)
                )
            else
                ( model, Cmd.none )


randomSide : Random.Generator CoinFlip.Side
randomSide =
    Random.uniform CoinFlip.Head [ CoinFlip.Tail ]



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
                    , option [ value "shop" ] [ text "🛒 Shop" ]
                    ]
                ]
            ]
            
        -- ROUTING
        , case model.currentPage of
            Dashboard ->
                Html.map (\(Dashboard.NavigateTo p) -> NavigateTo (mapPage p)) (Dashboard.view Dashboard.NavigateTo)

            CoinFlip ->
                CoinFlip.view model SelectCoinSide StartCoinSpin

            RussianRoulette ->
                Roulette.view model PullRouletteTrigger StartRouletteGame

            Leaderboard ->
                viewStaticPage "Bestenliste" "Hier entstehen bald die Highscores der reichsten Spieler!"

            Shop ->
                viewStaticPage "🛒 VIP Shop" "Hier kannst du bald virtuelle Goodies für deine Euro kaufen."

            GamePlaceholder id ->
                viewStaticPage ("Spiel " ++ String.fromInt id) "Dieses Spiel befindet sich aktuell noch in der Entwicklung!"
        ]


mapPage : Dashboard.Page -> Page
mapPage p =
    case p of
        Dashboard.Dashboard -> Dashboard
        Dashboard.CoinFlip -> CoinFlip
        Dashboard.GamePlaceholder id -> GamePlaceholder id


viewStaticPage : String -> String -> Html Msg
viewStaticPage titel beschreibung =
    div [ class "static-page" ]
        [ h2 [] [ text titel ]
        , p [] [ text beschreibung ]
        ]
