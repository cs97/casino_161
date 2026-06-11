module Components.Dashboard exposing (view)

import Html exposing (Html, button, div, h1, p, text)
import Html.Attributes exposing (class)
import Html.Events index exposing (onClick)


-- Wir importieren Typen, damit das Dashboard weiß, wohin es linken soll
type Page
    = Dashboard
    | CoinFlip
    | GamePlaceholder Int


type Msg
    = NavigateTo Page


view : (Page -> msg) -> Html msg
view toMsg =
    div []
        [ h1 [ class "casino-title" ] [ text "CASINO 161" ]
        , p [ class "casino-subtitle" ] [ text "Wähle ein Spiel und fordere dein Glück heraus!" ]
        , div [ class "game-grid" ]
            [ button [ class "game-card coin-card", onClick (toMsg CoinFlip) ] [ text "🪙 Drehmünze" ]
            , button [ class "game-card", onClick (toMsg (GamePlaceholder 2)) ] [ text "🎲 Spiel 2" ]
            , button [ class "game-card", onClick (toMsg (GamePlaceholder 3)) ] [ text "🃏 Spiel 3" ]
            , button [ class "game-card", onClick (toMsg (GamePlaceholder 4)) ] [ text "🚀 Spiel 4" ]
            , button [ class "game-card", onClick (toMsg (GamePlaceholder 5)) ] [ text "🎱 Spiel 5" ]
            , button [ class "game-card", onClick (toMsg (GamePlaceholder 6)) ] [ text "🎡 Spiel 6" ]
            , button [ class "game-card", onClick (toMsg (GamePlaceholder 7)) ] [ text "💥 Spiel 7" ]
            , button [ class "game-card", onClick (toMsg (GamePlaceholder 8)) ] [ text "💎 Spiel 8" ]
            ]
        ]
