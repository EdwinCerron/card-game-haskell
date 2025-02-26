{-# LANGUAGE DerivingStrategies #-}

-- | Spanish 40-card deck implementation
module Game.Card.Spanish
    ( SpanishSuit(..)
    , SpanishFace(..)
    , SpanishCard
    , newSpanishDeck
    ) where


import Game.Card.Types
import Game.Card.Deck (newDeck)
import GHC.Generics (Generic)

data SpanishSuit = Oros | Copas | Espadas | Bastos
    deriving stock (Eq, Show, Generic, Enum, Bounded)

data SpanishFace = As | Dos | Tres | Cuatro | Cinco | Seis | Siete 
                 | Sota | Caballo | Rey
    deriving stock (Eq, Show, Generic, Enum, Bounded)

type SpanishCard = Card SpanishSuit SpanishFace

instance HasValue SpanishCard where
    getValue card = case getFace card of
        As      -> 1
        Dos     -> 2
        Tres    -> 3
        Cuatro  -> 4
        Cinco   -> 5
        Seis    -> 6
        Siete   -> 7
        Sota    -> 10
        Caballo -> 11
        Rey     -> 12

instance HasColor SpanishCard where
    getColor card = case getSuit card of
        Oros  -> Red
        Copas -> Red
        _     -> Black

instance HasSuit SpanishCard SpanishSuit where
    getSuit = cardSuit

instance HasFace SpanishCard SpanishFace where
    getFace = cardFace

newSpanishDeck :: Deck SpanishCard
newSpanishDeck = newDeck Card
