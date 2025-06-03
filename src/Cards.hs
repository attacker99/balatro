{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

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

import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (Hashable)
import Data.Ord (comparing)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)

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

data Card
    = StoneCard
        { quantity :: Int
        , edition :: Edition
        , seal :: Maybe Seal
        }
    | NormalCard
        { rank :: Rank
        , suit :: Suit
        , quantity :: Int
        , enhancement :: Enhancement
        , edition :: Edition
        , seal :: Maybe Seal
        }
    deriving (Read, Show, Generic, Hashable, ToJSON, FromJSON)

instance Eq Card where
    (==) :: Card -> Card -> Bool
    StoneCard{} == StoneCard{} = True
    StoneCard{} == NormalCard{} = False
    NormalCard{} == StoneCard{} = False
    NormalCard{enhancement = e1, rank = r1, suit = s1} == NormalCard{enhancement = e2, rank = r2, suit = s2}
        | e1 == Wild || e2 == Wild = r1 == r2
        | otherwise = r1 == r2 && s1 == s2

instance Ord Card where
    compare :: Card -> Card -> Ordering
    compare card card' = if card == card' then EQ else comparing rank card card' <> comparing suit card card'

instance Enum Card where
    fromEnum :: Card -> Int
    fromEnum (NormalCard r s _ _ _ _) = fromEnum s * 13 + fromEnum r
    fromEnum StoneCard{} = 52

    toEnum :: Int -> Card
    toEnum n = let (s, r) = n `divMod` 13 in mkBaseCard (toEnum r) (toEnum s)

type Deck = Vector Card

allRanks :: [Rank]
allRanks = [Two .. Ace]

allSuits :: [Suit]
allSuits = [Spade .. Diamond]

mkBaseCard :: Rank -> Suit -> Card
mkBaseCard r s = NormalCard{rank = r, suit = s, quantity = 1, enhancement = None, edition = Base, seal = Nothing}

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
checkeredDeck =
    V.fromList
        [ mkBaseCard r s
        | r <- allRanks
        , s <- checkeredSuits
        ]

isEnhanced :: Enhancement -> Card -> Bool
isEnhanced property = \case
    StoneCard{} -> False
    NormalCard{enhancement = e} -> e == property

isWild :: Card -> Bool
isWild = isEnhanced Wild

isStone :: Card -> Bool
isStone = \case
    StoneCard{} -> True
    _ -> False
