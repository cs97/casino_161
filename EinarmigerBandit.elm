module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, h2, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random


-- 1. MODEL
-- Wir definieren die möglichen Symbole und den Zustand des Spiels.

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
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( { slot1 = Cherry
      , slot2 = Cherry
      , slot3 = Cherry
      , credits = 100
      , message = "Drücke auf Drehen! (Kostet 10 Credits)"
      }
    , Cmd.none
    )


-- 2. UPDATE

type Msg
    = Spin
    | NewSlots ( Symbol, Symbol, Symbol )

-- Ein Zufallsgenerator für unsere Symbole
symbolGenerator : Random.Generator Symbol
symbolGenerator =
    Random.uniform Cherry [ Seven, Diamond, Lemon ]

-- Ein Generator, der drei Symbole gleichzeitig würfelt
slotsGenerator : Random.Generator ( Symbol, Symbol, Symbol )
slotsGenerator =
    Random.map3 (\s1 s2 s3 -> ( s1, s2, s3 ))
        symbolGenerator
        symbolGenerator
        symbolGenerator

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Spin ->
            if model.credits < 10 then
                ( { model | message = "Nicht genug Credits! Spiel vorbei." }, Cmd.none )
            else
                -- Wir ziehen 10 Credits ab und starten den Zufallsgenerator
                ( { model | credits = model.credits - 10, message = "Die Walzen laufen..." }
                , Random.generate NewSlots slotsGenerator
                )

        NewSlots ( s1, s2, s3 ) ->
            let
                -- Gewinnberechnung
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
              }
            , Cmd.none
            )


-- 3. VIEW

-- Hilfsfunktion, um Symbole in Text/Emojis zu verwandeln
symbolToString : Symbol -> String
symbolToString symbol =
    case symbol of
        Cherry -> "🍒"
        Seven -> "7️⃣"
        Diamond -> "💎"
        Lemon -> "🍋"

view : Model -> Html Msg
view model =
    div [ style "text-align" "center", style "font-family" "sans-serif", style "margin-top" "50px" ]
        [ h1 [] [ text "🎰 Elm Einarmiger Bandit 🎰" ]
        
        -- Anzeige der Credits
        , h2 [ style "color" "green" ] [ text ("Credits: " ++ String.fromInt model.credits) ]
        
        -- Die Walzen (Slots)
        , div [ style "font-size" "70px", style "margin" "30px", style "letter-spacing" "20px" ]
            [ text (symbolToString model.slot1)
            , text (symbolToString model.slot2)
            , text (symbolToString model.slot3)
            ]
        
        -- Der Hebel / Button
        , div []
            [ button
                [ onClick Spin
                , style "font-size" "24px"
                , style "padding" "10px 30px"
                , style "background-color" "#ff4757"
                , style "color" "white"
                , style "border" "none"
                , style "border-radius" "5px"
                , style "cursor" "pointer"
                ]
                [ text "DREHEN!" ]
            ]
        
        -- Nachricht an den Spieler
        , div [ style "margin-top" "30px", style "font-size" "18px", style "font-weight" "bold" ]
            [ text model.message ]
        ]


-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
