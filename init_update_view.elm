module Main exposing (main)

import Browser
import Components.CoinFlip as CoinFlip
import Components.Dashboard as Dashboard
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
    | Leaderboard
    | Shop
    | GamePlaceholder Int


type alias Model =
    { currentPage : Page
    , balance : Int
    , dropdownOpen : Bool
    
    -- Zustände delegiert an die CoinFlip-Typen
    , coinSelection : CoinFlip.Side
    , coinGameState : CoinFlip.GameState
    , coinRotationDegrees : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { currentPage = Dashboard
      , balance = 100
      , dropdownOpen = False
      , coinSelection = CoinFlip.Head
      , coinGameState = CoinFlip.Idle
      , coinRotationDegrees = 0
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NavigateTo Page
    | SelectDropdown String
    | SelectCoinSide CoinFlip.Side
    | StartCoinSpin
    | CalculateCoinFlipResult CoinFlip.Side
    | RevealCoinResult { won : Bool, landedOn : CoinFlip.Side }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavigateTo page ->
            ( { model | currentPage = page }, Cmd.none )

        SelectDropdown val ->
            case val of
                "leaderboard" ->
                    ( { model | currentPage = Leaderboard }, Cmd.none )

                "shop" ->
                    ( { model | currentPage = Shop }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SelectCoinSide side ->
            case model.coinGameState of
                CoinFlip.Spinning ->
                    ( model, Cmd.none )

                _ ->
                    ( { model | coinSelection = side, coinGameState = CoinFlip.Idle }, Cmd.none )

        StartCoinSpin ->
            case model.coinGameState of
                CoinFlip.Spinning ->
                    ( model, Cmd.none )

                _ ->
                    ( { model | coinGameState = CoinFlip.Spinning }
                    , Random.generate CalculateCoinFlipResult randomSide 
                    )

        CalculateCoinFlipResult side ->
            let
                currentFullTurns =
                    model.coinRotationDegrees // 360

                targetExtra =
                    case side of
                        CoinFlip.Head ->
                            0

                        CoinFlip.Tail ->
                            180

                newRotation =
                    (currentFullTurns * 360) + 1800 + targetExtra

                won =
                    model.coinSelection == side

                resultData =
                    { won = won, landedOn = side }
            in
            ( { model | coinRotationDegrees = newRotation }
            , Process.sleep 2000 |> Task.perform (\_ -> RevealCoinResult resultData)
            )

        RevealCoinResult resultData ->
            let
                newBalance =
                    if resultData.won then
                        model.balance + 50
                    else
                        model.balance - 50
            in
            ( { model | coinGameState = CoinFlip.Result resultData, balance = newBalance }
            , Cmd.none 
            )


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
            
        -- SEITEN-ROUTING (Hier binden wir die ausgelagerten Views ein)
        , case model.currentPage of
            Dashboard ->
                -- Wir mappen das lokale Routing des Dashboards auf unsere Haupt-Msg
                Html.map (\(Dashboard.NavigateTo p) -> NavigateTo (mapPage p)) (Dashboard.view Dashboard.NavigateTo)

            CoinFlip ->
                CoinFlip.view model SelectCoinSide StartCoinSpin

            Leaderboard ->
                viewStaticPage "Bestenliste" "Hier entstehen bald die Highscores der reichsten Spieler!"

            Shop ->
                viewStaticPage "🛒 VIP Shop" "Hier kannst du bald virtuelle Goodies für deine Euro kaufen."

            GamePlaceholder id ->
                viewStaticPage ("Spiel " ++ String.fromInt id) "Dieses Spiel befindet sich aktuell noch in der Entwicklung!"
        ]


-- Hilfsfunktion, um den navigierten Typen aus dem Dashboard-Modul sauber zu übersetzen
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
