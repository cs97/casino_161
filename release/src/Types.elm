-- Types.elm
module Types exposing (..)

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

type Page
    = Dashboard
    | CoinFlip
    | RussianRoulette
    | RockPaperScissors
    | CardMonte
    | SlotMachine
    | Blackjack
    | WheelOfFortune
    | Leaderboard
    | ShopPage
    | GamePlaceholder Int

type alias Charm =
    { id : Int
    , name : String
    , multiplier : Float
    , price : Int
    , icon : String
    }