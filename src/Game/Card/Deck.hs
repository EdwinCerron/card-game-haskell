{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}

-- | Generic deck operations
module Game.Card.Deck
    ( newDeck
    , shuffle
    , draw
    , drawCard
    , cut
    ) where

import Game.Card.Types
import System.Random (RandomGen, randomR)
import Control.Monad.Random (MonadRandom, getRandom)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

-- Helper functions for shuffling
getRandomR :: MonadRandom m => (Int, Int) -> m Int
getRandomR (low, high) = do
    random <- getRandom
    return $ low + random `mod` (high - low + 1)

moveToFront :: Int -> [a] -> [a]
moveToFront n xs = 
    let (front, elem:back) = splitAt n xs
    in elem : (front ++ back)

fisherYatesShuffle :: MonadRandom m => [a] -> m [a]
fisherYatesShuffle [] = return []
fisherYatesShuffle xs = do
    let len = length xs
    idx <- getRandomR (0, len - 1)
    let (x:rest) = moveToFront idx xs
    shuffled <- fisherYatesShuffle rest
    return (x:shuffled)

-- | Create a new deck from bounded enumerable suits and faces
newDeck :: (Enum suit, Bounded suit, Enum face, Bounded face) 
        => (suit -> face -> card) 
        -> Deck card
newDeck constructor = Deck 
    [constructor s f | s <- [minBound..maxBound], f <- [minBound..maxBound]]

-- | Shuffle a deck using a random number generator
shuffle :: MonadRandom m => Deck a -> m (Deck a)
shuffle (Deck cs) = Deck <$> fisherYatesShuffle cs

-- | Draw n cards from the deck
draw :: Int -> Deck a -> Maybe (NonEmpty a, Deck a)
draw n (Deck cs)
    | n <= 0 || n > length cs = Nothing
    | otherwise = Just (NE.fromList taken, Deck remaining)
  where
    (taken, remaining) = splitAt n cs

-- | Draw a single card from the deck
drawCard :: Deck a -> (a, Deck a)
drawCard (Deck (x:xs)) = (x, Deck xs)
drawCard (Deck []) = error "Cannot draw from empty deck"

-- | Cut the deck at a specific position
cut :: Int -> Deck a -> Maybe (Deck a)
cut n deck@(Deck cs)
    | n <= 0 || n >= length cs = Nothing
    | otherwise = Just . Deck $ drop n cs ++ take n cs
