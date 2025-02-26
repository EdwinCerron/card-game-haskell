{-# LANGUAGE DerivingStrategies #-}

-- | Standard 52-card deck implementation
module Game.Card.Standard
    ( StandardSuit(..)
    , StandardFace(..)
    , StandardCard
    , newStandardDeck
    ) where

import GHC.Generics (Generic)
import Game.Card.Types
import Game.Card.Deck (newDeck)

data StandardSuit = Hearts | Diamonds | Clubs | Spades
    deriving stock (Eq, Show, Generic, Enum, Bounded)

data StandardFace = Ace | Two | Three | Four | Five | Six | Seven 
                 | Eight | Nine | Ten | Jack | Queen | King
    deriving stock (Eq, Show, Generic, Enum, Bounded)

type StandardCard = Card StandardSuit StandardFace

instance HasValue StandardCard where
    getValue card = case getFace card of
        Ace   -> 1
        King  -> 13
        Queen -> 12
        Jack  -> 11
        x     -> fromEnum x + 1

instance HasColor StandardCard where
    getColor card = case getSuit card of
        Hearts   -> Red
        Diamonds -> Red
        _        -> Black

instance HasSuit StandardCard StandardSuit where
    getSuit = cardSuit

instance HasFace StandardCard StandardFace where
    getFace = cardFace

newStandardDeck :: Deck StandardCard
newStandardDeck = newDeck Card
