-- SlotMachine.elm
module SlotMachine exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (Html, button, div, h2, p, text)
import Html.Attributes exposing (class, classList, disabled)
import Html.Events exposing (onClick)
import Random
import Time
import Types exposing (..)
import Utils exposing (slotsGenerator, symbolGenerator)

type alias Model =
    { slot1 : Symbol
    , slot2 : Symbol
    , slot3 : Symbol
    , message : String
    , isSpinning : Bool
    , spinTicks : Int
    }

init : ( Model, Cmd Msg )
init =
    ( { slot1 = Cherry
      , slot2 = Cherry
      , slot3 = Cherry
      , message = "Drücke auf Drehen! (Kostet 10 €)"
      , isSpinning = False
      , spinTicks = 0
      }
    , Cmd.none
    )

type Msg
    = StartSpin
    | Tick Time.Posix
    | NewSlots ( Symbol, Symbol, Symbol )

update : Msg -> Model -> ( Model, Cmd Msg, Int )
update msg model =
    case msg of
        StartSpin ->
            if model.isSpinning then
                ( model, Cmd.none, 0 )

            else
                ( { model
                    | isSpinning = True
                    , spinTicks = 0
                    , message = "Die Walzen laufen..."
                  }
                , Cmd.none
                , -10
                )

        Tick _ ->
            if model.spinTicks >= 10 then
                ( model, Random.generate NewSlots slotsGenerator, 0 )

            else
                ( { model | spinTicks = model.spinTicks + 1 }
                , Random.generate NewSlots slotsGenerator
                , 0
                )

        NewSlots ( s1, s2, s3 ) ->
            if model.isSpinning && model.spinTicks < 10 then
                ( { model | slot1 = s1, slot2 = s2, slot3 = s3 }
                , Cmd.none
                , 0
                )

            else
                let
                    ( finalS1, finalS2, finalS3 ) =
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
                    , message = msgText
                    , isSpinning = False
                  }
                , Cmd.none
                , winAmount
                )

subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isSpinning then
        Time.every 100 Tick

    else
        Sub.none

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

view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "🎰 Einarmiger Bandit" ]
        , div [ class "roulette-status" ] [ text model.message ]
        , div [ class "slot-arena" ]
            [ div
                [ classList
                    [ ( "slot-reel", True )
                    , ( "blur-animation", model.isSpinning )
                    ]
                ]
                [ text (symbolToString model.slot1) ]
            , div
                [ classList
                    [ ( "slot-reel", True )
                    , ( "blur-animation", model.isSpinning && model.spinTicks > 3 )
                    ]
                ]
                [ text (symbolToString model.slot2) ]
            , div
                [ classList
                    [ ( "slot-reel", True )
                    , ( "blur-animation", model.isSpinning && model.spinTicks > 6 )
                    ]
                ]
                [ text (symbolToString model.slot3) ]
            ]
        , div []
            [ button
                [ onClick StartSpin
                , class "btn action-btn"
                , disabled model.isSpinning
                ]
                [ text
                    (if model.isSpinning then
                        "Walzen drehen..."

                     else
                        "DREHEN! (10€)"
                    )
                ]
            ]
        ]