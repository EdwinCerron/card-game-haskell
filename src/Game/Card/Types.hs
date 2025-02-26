{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | Core types for card games
module Game.Card.Types
    ( -- * Type Classes
      HasValue(..)
    , HasColor(..)
    , HasSuit(..)
    , HasFace(..)
      -- * Common Types
    , Color(..)
    , Card(..)
    , Deck(..)
    ) where

import GHC.Generics (Generic)

-- | Basic card colors
data Color = Red | Black
    deriving stock (Eq, Show, Generic, Enum, Bounded)

-- | Generic card representation
data Card suit face = Card 
    { cardSuit :: suit  -- ^ Card's suit
    , cardFace :: face  -- ^ Card's face value
    } deriving stock (Eq, Show, Generic)

-- | Generic deck representation
newtype Deck card = Deck 
    { unDeck :: [card] 
    } deriving stock (Eq, Show)

-- | Cards that have a numeric value
class HasValue a where
    getValue :: a -> Int

-- | Cards that have a color
class HasColor a where
    getColor :: a -> Color

-- | Cards that have a suit
class HasSuit a s | a -> s where
    getSuit :: a -> s

-- | Cards that have a face value
class HasFace a f | a -> f where
    getFace :: a -> f
