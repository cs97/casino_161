-- Main.elm
module Main exposing (main)

import Api exposing (apiUrl, getScore, postScore)
import Blackjack as Bj
import Browser
import Browser.Navigation as Nav
import CardMonte as Monte
import CoinFlip as Coin
import Html exposing (Html, button, div, h1, h2, option, p, select, text)
import Html.Attributes exposing (class, classList, disabled, style, value)
import Html.Events exposing (onClick, onInput)
import Http
import Process
import Random
import RockPaperScissors as Rps
import RussianRoulette as Roulette
import Shop
import SlotMachine as Slot
import Svg exposing (Svg, circle, g, path, polygon, svg, text_)
import Svg.Attributes exposing (cx, cy, d, fill, fontSize, r, stroke, strokeWidth, textAnchor, transform, viewBox, x, y)
import Task
import Time
import Types exposing (..)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, int, map, oneOf, parse, s, string, top)
import Utils exposing (..)
import WheelOfFortune as Wheel

-- MAIN

main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }

-- ROUTING

type Route
    = RouteDashboard
    | RouteCoinFlip
    | RouteRussianRoulette
    | RouteRockPaperScissors
    | RouteCardMonte
    | RouteSlotMachine
    | RouteBlackjack
    | RouteWheelOfFortune
    | RouteLeaderboard
    | RouteShop
    | RouteGamePlaceholder Int
    | RouteNotFound

routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map RouteDashboard top
        , map RouteCoinFlip (s "coinflip")
        , map RouteRussianRoulette (s "roulette")
        , map RouteRockPaperScissors (s "rps")
        , map RouteCardMonte (s "monte")
        , map RouteSlotMachine (s "slot")
        , map RouteBlackjack (s "blackjack")
        , map RouteWheelOfFortune (s "wheel")
        , map RouteLeaderboard (s "leaderboard")
        , map RouteShop (s "shop")
        , map RouteGamePlaceholder (s "game" </> int)
        ]

routeToPage : Route -> Page
routeToPage route =
    case route of
        RouteDashboard ->
            Dashboard

        RouteCoinFlip ->
            CoinFlip

        RouteRussianRoulette ->
            RussianRoulette

        RouteRockPaperScissors ->
            RockPaperScissors

        RouteCardMonte ->
            CardMonte

        RouteSlotMachine ->
            SlotMachine

        RouteBlackjack ->
            Blackjack

        RouteWheelOfFortune ->
            WheelOfFortune

        RouteLeaderboard ->
            Leaderboard

        RouteShop ->
            ShopPage

        RouteGamePlaceholder id ->
            GamePlaceholder id

        RouteNotFound ->
            Dashboard

pageToRoute : Page -> String
pageToRoute page =
    case page of
        Dashboard ->
            "/"

        CoinFlip ->
            "/coinflip"

        RussianRoulette ->
            "/roulette"

        RockPaperScissors ->
            "/rps"

        CardMonte ->
            "/monte"

        SlotMachine ->
            "/slot"

        Blackjack ->
            "/blackjack"

        WheelOfFortune ->
            "/wheel"

        Leaderboard ->
            "/leaderboard"

        ShopPage ->
            "/shop"

        GamePlaceholder id ->
            "/game/" ++ String.fromInt id

-- MODEL

type alias Model =
    { key : Nav.Key
    , currentPage : Page
    , balance : Int
    , dropdownValue : String
    , coin : Coin.Model
    , roulette : Roulette.Model
    , rps : Rps.Model
    , monte : Monte.Model
    , slot : Slot.Model
    , blackjack : Bj.Model
    , wheel : Wheel.Model
    , shop : Shop.Model
    }

init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        route =
            parse routeParser url
                |> Maybe.withDefault RouteNotFound

        page =
            routeToPage route

        ( coin, coinCmd ) =
            Coin.init

        ( roulette, rouletteCmd ) =
            Roulette.init

        ( rps, rpsCmd ) =
            Rps.init

        ( monte, monteCmd ) =
            Monte.init

        ( slot, slotCmd ) =
            Slot.init

        ( blackjack, bjCmd ) =
            Bj.init

        ( wheel, wheelCmd ) =
            Wheel.init

        ( shop, shopCmd ) =
            Shop.init
    in
    ( { key = key
      , currentPage = page
      , balance = 100
      , dropdownValue = ""
      , coin = coin
      , roulette = roulette
      , rps = rps
      , monte = monte
      , slot = slot
      , blackjack = blackjack
      , wheel = wheel
      , shop = shop
      }
    , Cmd.batch
        [ getScore GotInitialScore
        , Cmd.map CoinMsg coinCmd
        , Cmd.map RouletteMsg rouletteCmd
        , Cmd.map RpsMsg rpsCmd
        , Cmd.map MonteMsg monteCmd
        , Cmd.map SlotMsg slotCmd
        , Cmd.map BjMsg bjCmd
        , Cmd.map WheelMsg wheelCmd
        , Cmd.map ShopMsg shopCmd
        ]
    )

-- UPDATE

type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | NavigateTo Page
    | SelectDropdown String
    | GotInitialScore (Result Http.Error Int)
    | ScorePosted (Result Http.Error Int)
    | CoinMsg Coin.Msg
    | RouletteMsg Roulette.Msg
    | RpsMsg Rps.Msg
    | MonteMsg Monte.Msg
    | SlotMsg Slot.Msg
    | BjMsg Bj.Msg
    | WheelMsg Wheel.Msg
    | ShopMsg Shop.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                route =
                    parse routeParser url
                        |> Maybe.withDefault RouteNotFound

                page =
                    routeToPage route

                newModel =
                    { model | currentPage = page, dropdownValue = "" }
            in
            case page of
                RussianRoulette ->
                    let
                        ( newRoulette, rouletteCmd ) =
                            Roulette.init
                    in
                    ( { newModel | roulette = newRoulette }
                    , Cmd.map RouletteMsg rouletteCmd
                    )

                RockPaperScissors ->
                    let
                        ( newRps, rpsCmd ) =
                            Rps.init
                    in
                    ( { newModel | rps = newRps }
                    , Cmd.map RpsMsg rpsCmd
                    )

                CardMonte ->
                    let
                        ( newMonte, monteCmd ) =
                            Monte.init
                    in
                    ( { newModel | monte = newMonte }
                    , Cmd.map MonteMsg monteCmd
                    )

                SlotMachine ->
                    let
                        ( newSlot, slotCmd ) =
                            Slot.init
                    in
                    ( { newModel | slot = newSlot }
                    , Cmd.map SlotMsg slotCmd
                    )

                Blackjack ->
                    let
                        ( newBlackjack, bjCmd ) =
                            Bj.init
                    in
                    ( { newModel | blackjack = newBlackjack }
                    , Cmd.map BjMsg bjCmd
                    )

                WheelOfFortune ->
                    let
                        ( newWheel, wheelCmd ) =
                            Wheel.init
                    in
                    ( { newModel | wheel = newWheel }
                    , Cmd.map WheelMsg wheelCmd
                    )

                _ ->
                    ( newModel, Cmd.none )

        NavigateTo page ->
            let
                path =
                    pageToRoute page
            in
            ( { model | currentPage = page, dropdownValue = "" }
            , Nav.pushUrl model.key path
            )

        SelectDropdown val ->
            case val of
                "leaderboard" ->
                    ( { model | currentPage = Leaderboard, dropdownValue = "" }
                    , Nav.pushUrl model.key "/leaderboard"
                    )

                "shop" ->
                    ( { model | currentPage = ShopPage, dropdownValue = "" }
                    , Nav.pushUrl model.key "/shop"
                    )

                _ ->
                    ( { model | dropdownValue = "" }, Cmd.none )

        GotInitialScore result ->
            case result of
                Ok score ->
                    ( { model | balance = score }, Cmd.none )

                Err _ ->
                    ( { model | balance = 100 }, Cmd.none )

        ScorePosted result ->
            case result of
                Ok score ->
                    ( { model | balance = score }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        -- COIN FLIP
        CoinMsg subMsg ->
            let
                ( newCoin, coinCmd, balanceChange ) =
                    Coin.update subMsg model.coin
            in
            ( { model
                | coin = newCoin
                , balance = model.balance + balanceChange
              }
            , Cmd.batch
                [ Cmd.map CoinMsg coinCmd
                , postScore (model.balance + balanceChange) ScorePosted
                ]
            )

        -- RUSSIAN ROULETTE
        RouletteMsg subMsg ->
            let
                ( newRoulette, rouletteCmd, balanceChange ) =
                    Roulette.update subMsg model.roulette
            in
            ( { model
                | roulette = newRoulette
                , balance = model.balance + balanceChange
              }
            , Cmd.batch
                [ Cmd.map RouletteMsg rouletteCmd
                , postScore (model.balance + balanceChange) ScorePosted
                ]
            )

        -- ROCK PAPER SCISSORS
        RpsMsg subMsg ->
            let
                ( newRps, rpsCmd, balanceChange ) =
                    Rps.update subMsg model.rps
            in
            ( { model
                | rps = newRps
                , balance = model.balance + balanceChange
              }
            , Cmd.batch
                [ Cmd.map RpsMsg rpsCmd
                , postScore (model.balance + balanceChange) ScorePosted
                ]
            )

        -- CARD MONTE
        MonteMsg subMsg ->
            let
                ( newMonte, monteCmd, balanceChange ) =
                    Monte.update subMsg model.monte
            in
            ( { model
                | monte = newMonte
                , balance = model.balance + balanceChange
              }
            , Cmd.batch
                [ Cmd.map MonteMsg monteCmd
                , postScore (model.balance + balanceChange) ScorePosted
                ]
            )

        -- SLOT MACHINE
        SlotMsg subMsg ->
            let
                ( newSlot, slotCmd, balanceChange ) =
                    Slot.update subMsg model.slot
            in
            ( { model
                | slot = newSlot
                , balance = model.balance + balanceChange
              }
            , Cmd.batch
                [ Cmd.map SlotMsg slotCmd
                , postScore (model.balance + balanceChange) ScorePosted
                ]
            )

        -- BLACKJACK
        BjMsg subMsg ->
            let
                ( newBj, bjCmd, balanceChange ) =
                    Bj.update subMsg model.blackjack
            in
            ( { model
                | blackjack = newBj
                , balance = model.balance + balanceChange
              }
            , Cmd.batch
                [ Cmd.map BjMsg bjCmd
                , postScore (model.balance + balanceChange) ScorePosted
                ]
            )

        -- WHEEL OF FORTUNE
        WheelMsg subMsg ->
            let
                ( newWheel, wheelCmd, balanceChange ) =
                    Wheel.update subMsg model.wheel
            in
            ( { model
                | wheel = newWheel
                , balance = model.balance + balanceChange
              }
            , Cmd.batch
                [ Cmd.map WheelMsg wheelCmd
                , postScore (model.balance + balanceChange) ScorePosted
                ]
            )

        -- SHOP
        ShopMsg subMsg ->
            let
                ( newShop, shopCmd, balanceChange ) =
                    Shop.update subMsg model.shop
            in
            ( { model
                | shop = newShop
                , balance = model.balance + balanceChange
              }
            , Cmd.batch
                [ Cmd.map ShopMsg shopCmd
                , postScore (model.balance + balanceChange) ScorePosted
                ]
            )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map CoinMsg (Coin.subscriptions model.coin)
        , Sub.map SlotMsg (Slot.subscriptions model.slot)
        , Sub.map BjMsg (Bj.subscriptions model.blackjack)
        ]

-- VIEW

view : Model -> Browser.Document Msg
view model =
    { title = "Casino 161 - " ++ pageTitle model.currentPage
    , body =
        [ div
            [ style "width" "100vw"
            , style "height" "100vh"
            , style "display" "flex"
            , style "justify-content" "center"
            , style "align-items" "center"
            , style "position" "relative"
            ]
            [ viewTopBar model
            , div
                [ classList
                    [ ( "game-container", True )
                    , ( "dashboard-active", model.currentPage == Dashboard )
                    ]
                ]
                [ viewPage model
                ]
            ]
        ]
    }

pageTitle : Page -> String
pageTitle page =
    case page of
        Dashboard ->
            "Startseite"

        CoinFlip ->
            "Drehmünze"

        RussianRoulette ->
            "Russisch Roulette"

        RockPaperScissors ->
            "Schere Stein Papier"

        CardMonte ->
            "Find the Lady"

        SlotMachine ->
            "Einarmiger Bandit"

        Blackjack ->
            "Blackjack"

        WheelOfFortune ->
            "Glücksrad"

        Leaderboard ->
            "Bestenliste"

        ShopPage ->
            "Shop"

        GamePlaceholder id ->
            "Spiel " ++ String.fromInt id

viewTopBar : Model -> Html Msg
viewTopBar model =
    div [ class "top-bar" ]
        [ button [ class "nav-home-btn", onClick (NavigateTo Dashboard) ] [ text "Home" ]
        , div [ class "top-right-controls" ]
            [ div [ class "charm-indicator" ]
                [ text
                    (Shop.getActiveCharmIcon model.shop
                        ++ " "
                        ++ Shop.getActiveCharmName model.shop
                        ++ " ("
                        ++ String.fromFloat (Shop.getActiveMultiplier model.shop)
                        ++ "x)"
                    )
                ]
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

viewPage : Model -> Html Msg
viewPage model =
    case model.currentPage of
        Dashboard ->
            viewDashboard

        CoinFlip ->
            Html.map CoinMsg (Coin.view model.coin)

        RussianRoulette ->
            Html.map RouletteMsg (Roulette.view model.roulette)

        RockPaperScissors ->
            Html.map RpsMsg (Rps.view model.rps)

        CardMonte ->
            Html.map MonteMsg (Monte.view model.monte)

        SlotMachine ->
            Html.map SlotMsg (Slot.view model.slot)

        Blackjack ->
            Html.map BjMsg (Bj.view model.blackjack)

        WheelOfFortune ->
            Html.map WheelMsg (Wheel.view model.wheel)

        Leaderboard ->
            viewStaticPage "Bestenliste" "Hier entstehen bald die Highscores der reichsten Spieler!"

        ShopPage ->
            Html.map ShopMsg (Shop.view model.shop)

        GamePlaceholder id ->
            viewStaticPage ("Spiel " ++ String.fromInt id) "Dieses Spiel befindet sich aktuell noch in der Entwicklung!"

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

viewStaticPage : String -> String -> Html Msg
viewStaticPage titel beschreibung =
    div [ class "static-page" ]
        [ h2 [] [ text titel ]
        , p [] [ text beschreibung ]
        ]
