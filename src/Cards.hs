{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}

module Cards (
    Rank (..),
    Suit (..),
    Enhancement (..),
    Edition (..),
    Seal (..),
    Card (..),
    Deck,
    stdDeck,
    abandonedDeck,
    checkeredDeck,
    mkBaseCard,
    allRanks,
    allSuits,
    isWild,
    isStone,
)
where

import Data.Ord (comparing)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Rank
    = One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace
    deriving (Bounded, Enum, Eq, Ord, Read, Show, Generic, Hashable, ToJSON, FromJSON)

data Suit
    = Spade
    | Heart
    | Club
    | Diamond
    deriving (Bounded, Enum, Eq, Ord, Read, Show, Generic, Hashable, ToJSON, FromJSON)

data Enhancement
    = None
    | Bonus
    | Mult
    | Wild
    | Glass
    | Steel
    | Stone
    | Gold
    | Lucky
    deriving (Bounded, Enum, Eq, Read, Show, Generic, Hashable, ToJSON, FromJSON)

data Edition
    = Base
    | Foil
    | Holographic
    | Polychrome
    deriving (Bounded, Enum, Eq, Read, Show, Generic, Hashable, ToJSON, FromJSON)

data Seal
    = GoldS
    | Red
    | Blue
    | Purple
    deriving (Bounded, Enum, Eq, Read, Show, Generic, Hashable, ToJSON, FromJSON)

data Card = Card
    { rank :: Rank
    , suit :: Suit
    , enhancement :: Enhancement
    , edition :: Edition
    , seal :: Maybe Seal
    }
    deriving (Read, Show, Generic, Hashable, ToJSON, FromJSON)

instance Eq Card where
    (==) :: Card -> Card -> Bool
    Card r s ehmnt _ _ == Card r' s' ehmnt' _ _ =
        case ehmnt of
            Stone -> ehmnt' == Stone
            Wild -> r == r'
            _ -> case ehmnt' of
                Stone -> False
                Wild -> r == r'
                _ -> r == r' && s == s'

instance Ord Card where
    compare :: Card -> Card -> Ordering
    compare card card' = if card == card' then EQ else comparing rank card card' <> comparing suit card card'

instance Enum Card where
    fromEnum :: Card -> Int
    fromEnum (Card r s _ _ _) = fromEnum s * 13 + fromEnum r

    toEnum :: Int -> Card
    toEnum n = let (s, r) = n `divMod` 13 in mkBaseCard (toEnum r) (toEnum s)

type Deck = Vector Card

allRanks :: [Rank]
allRanks = [Two .. Ace]

allSuits :: [Suit]
allSuits = [Spade .. Diamond]

mkBaseCard :: Rank -> Suit -> Card
mkBaseCard r s = Card r s None Base Nothing

stdDeck :: Deck
stdDeck =
    V.fromList
        [ mkBaseCard r s
        | r <- allRanks
        , s <- allSuits
        ]

abandonedRanks :: [Rank]
abandonedRanks = [Two .. Ten] ++ [Ace]

abandonedDeck :: Deck
abandonedDeck =
    V.fromList
        [ mkBaseCard r s
        | r <- abandonedRanks
        , s <- allSuits
        ]

checkeredSuits :: [Suit]
checkeredSuits = [Spade, Heart]

checkeredDeck :: Vector Card
checkeredDeck = V.fromList
        [ mkBaseCard r s
        | r <- allRanks
        , s <- checkeredSuits
        ]

isEnhanced :: Enhancement -> Card -> Bool
isEnhanced property =
    (== property) . enhancement

isWild :: Card -> Bool
isWild = isEnhanced Wild

isStone :: Card -> Bool
isStone = isEnhanced Stone
