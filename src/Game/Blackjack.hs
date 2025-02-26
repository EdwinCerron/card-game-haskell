{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Blackjack
    ( BlackjackGame(..)
    , Player(..)
    , GameState(..)
    , newGame
    , hit
    , stand
    , isGameOver
    , getWinner
    ) where

import Game.Card.Standard
import Game.Card.Types (Card(..), Deck(..), getFace)
import Game.Card.Deck (drawCard)
import Data.List (find)

data Player = Player
    { playerHand :: [StandardCard]
    , playerScore :: Int
    } deriving stock (Show, Eq)

data GameState = InProgress | DealerTurn | GameOver
    deriving stock (Show, Eq)

data BlackjackGame = BlackjackGame
    { gamePlayer :: Player
    , gameDealer :: Player
    , gameDeck :: Deck StandardCard
    , gameState :: GameState
    } deriving stock (Show)

calculateScore :: [StandardCard] -> Int
calculateScore cards = 
    let baseScore = sum $ map cardValue cards
        numAces = length $ filter isAce cards
        cardValue c = case getFace c of
            Ace -> 11
            King -> 10
            Queen -> 10
            Jack -> 10
            x -> min (fromEnum x + 1) 10
        isAce c = getFace c == Ace
    in adjustForAces baseScore numAces
  where
    adjustForAces score aces
        | score <= 21 = score
        | aces > 0 = adjustForAces (score - 10) (aces - 1)
        | otherwise = score

newPlayer :: Player
newPlayer = Player [] 0

newGame :: Deck StandardCard -> BlackjackGame
newGame deck = 
    let (card1, deck1) = drawCard deck
        (card2, deck2) = drawCard deck1
        (card3, deck3) = drawCard deck2
        (card4, deck4) = drawCard deck3
        playerHand = [card1, card2]
        dealerHand = [card3, card4]
    in BlackjackGame
        { gamePlayer = Player playerHand (calculateScore playerHand)
        , gameDealer = Player dealerHand (calculateScore dealerHand)
        , gameDeck = deck4
        , gameState = InProgress
        }

hit :: BlackjackGame -> BlackjackGame
hit game@BlackjackGame{..}
    | gameState /= InProgress = game
    | otherwise = 
        let (card, newDeck) = drawCard gameDeck
            newPlayerHand = card : playerHand gamePlayer
            newScore = calculateScore newPlayerHand
            newPlayer = Player newPlayerHand newScore
            newState = if newScore > 21 then GameOver else InProgress
        in game
            { gamePlayer = newPlayer
            , gameDeck = newDeck
            , gameState = newState
            }

stand :: BlackjackGame -> BlackjackGame
stand game@BlackjackGame{..} = 
    let finalGame = dealerPlay game{gameState = DealerTurn}
    in finalGame{gameState = GameOver}

dealerPlay :: BlackjackGame -> BlackjackGame
dealerPlay game@BlackjackGame{..}
    | dealerScore >= 17 = game
    | otherwise =
        let (card, newDeck) = drawCard gameDeck
            newDealerHand = card : playerHand gameDealer
            newScore = calculateScore newDealerHand
            newDealer = Player newDealerHand newScore
        in dealerPlay game
            { gameDealer = newDealer
            , gameDeck = newDeck
            }
    where
        dealerScore = playerScore gameDealer

isGameOver :: BlackjackGame -> Bool
isGameOver game = gameState game == GameOver

getWinner :: BlackjackGame -> Maybe Player
getWinner BlackjackGame{..}
    | playerScore gamePlayer > 21 = Just gameDealer
    | playerScore gameDealer > 21 = Just gamePlayer
    | playerScore gamePlayer > playerScore gameDealer = Just gamePlayer
    | playerScore gameDealer > playerScore gamePlayer = Just gameDealer
    | otherwise = Nothing  -- Empate
