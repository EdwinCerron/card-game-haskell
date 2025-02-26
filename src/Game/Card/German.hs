{-# LANGUAGE DerivingStrategies #-}

-- | German 32-card deck implementation
module Game.Card.German
    ( GermanSuit(..)
    , GermanFace(..)
    , GermanCard
    , newGermanDeck
    ) where

import Game.Card.Types
import Game.Card.Deck (newDeck)
import GHC.Generics (Generic)

data GermanSuit = Eichel | Grun | Herz | Schellen
    deriving stock (Eq, Show, Generic, Enum, Bounded)

data GermanFace = Sieben | Acht | Neun | Zehn | Unter | Ober | Konig | Ass
    deriving stock (Eq, Show, Generic, Enum, Bounded)

type GermanCard = Card GermanSuit GermanFace

instance HasValue GermanCard where
    getValue card = case getFace card of
        Sieben -> 7
        Acht   -> 8
        Neun   -> 9
        Zehn   -> 10
        Unter  -> 11
        Ober   -> 12
        Konig  -> 13
        Ass    -> 14

instance HasColor GermanCard where
    getColor card = case getSuit card of
        Herz     -> Red
        Schellen -> Red
        _        -> Black

instance HasSuit GermanCard GermanSuit where
    getSuit = cardSuit

instance HasFace GermanCard GermanFace where
    getFace = cardFace

newGermanDeck :: Deck GermanCard
newGermanDeck = newDeck Card
