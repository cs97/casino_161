module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, h2, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Random
import Time


-- 1. MODEL

type Symbol
    = Cherry
    | Seven
    | Diamond
    | Lemon

type alias Model =
    { slot1 : Symbol
    , slot2 : Symbol
    , slot3 : Symbol
    , credits : Int
    , message : String
    , isSpinning : Bool    -- Läuft das Spiel gerade?
    , spinTicks : Int      -- Zähler für die Animationsdauer
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( { slot1 = Cherry
      , slot2 = Cherry
      , slot3 = Cherry
      , credits = 100
      , message = "Drücke auf Drehen! (Kostet 10 Credits)"
      , isSpinning = False
      , spinTicks = 0
      }
    , Cmd.none
    )


-- 2. UPDATE

type Msg
    = StartSpin
    | Tick Time.Posix
    | NewSlots ( Symbol, Symbol, Symbol )

symbolGenerator : Random.Generator Symbol
symbolGenerator =
    Random.uniform Cherry [ Seven, Diamond, Lemon ]

slotsGenerator : Random.Generator ( Symbol, Symbol, Symbol )
slotsGenerator =
    Random.map3 (\s1 s2 s3 -> ( s1, s2, s3 ))
        symbolGenerator
        symbolGenerator
        symbolGenerator

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartSpin ->
            if model.isSpinning then
                ( model, Cmd.none ) -- Ignorieren, wenn es schon läuft
            else if model.credits < 10 then
                ( { model | message = "Nicht genug Credits! Spiel vorbei." }, Cmd.none )
            else
                ( { model
                    | credits = model.credits - 10
                    , message = "Die Walzen laufen..."
                    , isSpinning = True
                    , spinTicks = 0
                  }
                , Cmd.none
                )

        Tick _ ->
            if model.spinTicks >= 10 then
                -- Nach 10 Ticks (ca. 1 Sekunde) stoppen wir und holen das Endergebnis
                ( model, Random.generate NewSlots slotsGenerator )
            else
                -- Während des Drehens würfeln wir ständig Zwischenergebnisse für den visuellen Effekt
                ( { model | spinTicks = model.spinTicks + 1 }, Random.generate NewSlots slotsGenerator )

        NewSlots ( s1, s2, s3 ) ->
            if model.isSpinning && model.spinTicks < 10 then
                -- Animation läuft noch: Nur Symbole wild austauschen
                ( { model | slot1 = s1, slot2 = s2, slot3 = s3 }, Cmd.none )
            else
                -- Finale Auswertung
                let
                    ( winAmount, msgText ) =
                        if s1 == s2 && s2 == s3 then
                            case s1 of
                                Seven -> ( 100, "JACKPOT! 3 Siebenen! +100 Credits!" )
                                Diamond -> ( 60, "Wow! 3 Diamanten! +60 Credits!" )
                                Cherry -> ( 40, "Süß! 3 Kirschen! +40 Credits!" )
                                Lemon -> ( 30, "Sauer bringt Geld! 3 Zitronen! +30 Credits!" )
                        else if s1 == s2 || s2 == s3 || s1 == s3 then
                            ( 15, "Paar! +15 Credits." )
                        else
                            ( 0, "Leider verloren. Versuch es noch einmal!" )
                in
                ( { model
                    | slot1 = s1
                    , slot2 = s2
                    , slot3 = s3
                    , credits = model.credits + winAmount
                    , message = msgText
                    , isSpinning = False
                  }
                , Cmd.none
                )


-- 3. SUBSCRIPTIONS
-- Hier sagen wir Elm, dass wir jede 100 Millisekunden ein Signal wollen, ABER NUR wenn die Walzen drehen.

subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isSpinning then
        Time.every 100 Tick
    else
        Sub.none


-- 4. VIEW

symbolToString : Symbol -> String
symbolToString symbol =
    case symbol of
        Cherry -> "🍒"
        Seven -> "7️⃣"
        Diamond -> "💎"
        Lemon -> "🍋"

view : Model -> Html Msg
view model =
    -- Hier aktivieren wir deinen CSS-Container
    div [ class "game-container" ]
        [ h1 [] [ text "🎰 Elm Bandit 🎰" ]
        , h2 [ class "balance-display-text" ] [ text ("Credits: " ++ String.fromInt model.credits) ]
        
        -- Die Walzenbox
        , div [ style "display" "flex", style "justify-content" "center", style "gap" "20px", style "margin" "30px 0", style "font-size" "70px" ]
            [ -- Walze 1 stoppt sofort am Ende
              div [ class (if model.isSpinning then "blur-animation" else "") ] [ text (symbolToString model.slot1) ]
              -- Walze 2 dreht gefühlt etwas länger
            , div [ class (if model.isSpinning && model.spinTicks > 3 then "blur-animation" else "") ] [ text (symbolToString model.slot2) ]
              -- Walze 3 dreht am längsten
            , div [ class (if model.isSpinning && model.spinTicks > 6 then "blur-animation" else "") ] [ text (symbolToString model.slot3) ]
            ]
        
        , div []
            [ button
                [ onClick StartSpin
                , class (if model.isSpinning then "disabled-btn" else "spin-btn")
                ]
                [ text (if model.isSpinning then "Mische..." else "DREHEN!") ]
            ]
        
        , div [ style "margin-top" "30px", style "font-size" "18px", style "font-weight" "bold", style "color" "#fff" ]
            [ text model.message ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
