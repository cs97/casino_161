-- Utils.elm
module Utils exposing (randomRPS, randomShuffleList, randomShuffleType, randomSide, bjCardGenerator, bjCardValue, bjCalculateScore, symbolGenerator, slotsGenerator, wheelSectors)

import Random
import Types exposing (..)

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

bjCardValue : BjCard -> Int
bjCardValue card =
    case card of
        BjAce -> 11
        BjTwo -> 2
        BjThree -> 3
        BjFour -> 4
        BjFive -> 5
        BjSix -> 6
        BjSeven -> 7
        BjEight -> 8
        BjNine -> 9
        BjTen -> 10
        BjJack -> 10
        BjQueen -> 10
        BjKing -> 10

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

wheelSectors : List WheelSector
wheelSectors =
    [ { id = 0, label = "JACKPOT", multiplier = 7.5, color = "#ffcc00", textCol = "#000" }
    , { id = 1, label = "NIETE", multiplier = 0.0, color = "#d9534f", textCol = "#fff" }
    , { id = 2, label = "2x GEWINN", multiplier = 2.0, color = "#5cb85c", textCol = "#fff" }
    , { id = 3, label = "NIETE", multiplier = 0.0, color = "#d9534f", textCol = "#fff" }
    , { id = 4, label = "3x GEWINN", multiplier = 3.0, color = "#f0ad4e", textCol = "#fff" }
    , { id = 5, label = "NIETE", multiplier = 0.0, color = "#d9534f", textCol = "#fff" }
    , { id = 6, label = "4x GEWINN", multiplier = 4.0, color = "#5bc0de", textCol = "#fff" }
    , { id = 7, label = "NIETE", multiplier = 0.0, color = "#d9534f", textCol = "#fff" }
    ]