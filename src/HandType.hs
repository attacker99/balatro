{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HandType (
    HandType (..),
    Hand,
    Sz,
    Dist,
    allHandtypes,
    getHandType,
    checkHandType,
)
where

import Cards
import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.Aeson (ToJSON)
import Data.Foldable (toList)
import Data.List (find, foldl', partition)
import Data.Sequence (Seq (..), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Debug.Trace (traceShow)
import GHC.Generics (Generic)

data HandType
    = FlushFive
    | FlushHouse
    | FiveKind
    | StraightFlush
    | FourKind
    | FullHouse
    | Flush
    | Straight
    | ThreeKind
    | TwoPair
    | Pair
    | HighCard
    deriving (Enum, Eq, Ord, Read, Show, Generic, ToJSON)

type Hand = [Card]
type Dist = Int
type Sz = Int
type Counter = Vector Int
type Indices = [Int]
type CntStats = (Indices, Indices, Indices, Indices, Indices)
type RankEnum = Int

allHandtypes :: [HandType]
allHandtypes = [FlushFive .. HighCard]

getHandType :: Hand -> Sz -> Dist -> HandType
getHandType cards fs_sz dist =
    case find (checkHandType cards fs_sz dist) allHandtypes of
        Just ht -> ht
        _ -> error "no handtype found"

checkHandType :: Hand -> Sz -> Dist -> HandType -> Bool
checkHandType cards fs_sz dist hand_type =
    traceShow ("Combined cards: " ++ show combined_cards) $
        traceShow ("Suit counts: " ++ show (V.toList suit_cnt)) $
            traceShow ("Rank counts: " ++ show (V.toList rank_cnt)) $
                traceShow ("Rank count stats: " ++ show rank_cnt_stats) $
                    traceShow ("Rank count stats by suit: " ++ show (map show (V.toList rank_cnt_stats_by_suit))) $
                        case hand_type of
                            FlushFive -> any isFlushFive rank_cnt_stats_by_suit
                            FlushHouse -> any isFlushHouse rank_cnt_stats_by_suit
                            FiveKind -> not (null cntm5)
                            FourKind -> not (null cnt4)
                            FullHouse -> isFullHouse rank_cnt_stats
                            Flush -> any (>= fs_sz) suit_cnt
                            StraightFlush ->
                                any (\st_enum -> suit_cnt V.! st_enum >= fs_sz && isStraightFlush (rank_enums_by_suit V.! st_enum)) [0 .. 3]
                            Straight -> any isStraight (genSlides fs_sz rank_enums)
                            ThreeKind -> not (null cnt3)
                            TwoPair -> length cnt2 >= 2
                            Pair -> not (null cnt2)
                            HighCard -> True
  where
    (_, nstones) = partition isStone cards
    (wilds, nwilds) = partition isWild nstones
    combined_cards = nwilds ++ genWilds wilds
    suit_cnt = genCounterSuit combined_cards
    rank_cnt = genCounterRank nstones
    rank_cnt_stats@(_, cnt2, cnt3, cnt4, cntm5) = categorizeIndices rank_cnt
    cards_by_suit = genCardsBySuit combined_cards
    rank_cnt_by_suit = genCounterRank <$> cards_by_suit
    rank_cnt_stats_by_suit = categorizeIndices <$> rank_cnt_by_suit
    rank_enums_by_suit = getRankEnums <$> rank_cnt_by_suit
    rank_enums = getRankEnums rank_cnt

    genWilds :: [Card] -> [Card]
    genWilds = concatMap (\card -> concatMap (replicate (quantity card) . mkBaseCard (rank card)) allSuits)

    getRankEnums :: Counter -> [RankEnum]
    getRankEnums cnter = filter (\rank_enum -> cnter V.! rank_enum > 0) [0 .. 13]

    isFlushMissing :: Indices -> Sz -> Bool
    isFlushMissing cnter req_cnt = fs_sz == 4 && any hasEnoughRanks cnter
      where
        hasEnoughRanks rank_enum = rank_cnt V.! rank_enum >= req_cnt

    isFlushFive :: CntStats -> Bool
    isFlushFive (_, _, _, c4, cm5) = not (null cm5) || isFlushMissing c4 5

    isFlushHouse :: CntStats -> Bool
    isFlushHouse (c1, c2, c3, c4, cm5) = (cm3_sz > 0 && cm3_sz + length c2 >= 2) || isFlushHouseMissing
      where
        cm3_sz = length c3 + length c4 + length cm5
        isFlushHouseMissing = (length c2 >= 2 && isFlushMissing c2 3) || (cm3_sz >= 1 && isFlushMissing c1 2)

    isFullHouse :: CntStats -> Bool
    isFullHouse (_, c2, c3, c4, cm5) = cm3_sz > 0 && cm3_sz + length c2 >= 2
      where
        cm3_sz = length c3 + length c4 + length cm5

    isStraightFlush :: [RankEnum] -> Bool
    isStraightFlush suited_rk_enums = any isStraight (genSlides fs_sz suited_rk_enums) || isStraightFlushMissing
      where
        isStraightFlushMissing = fs_sz == 4 && any isCompletable (genSlides 3 suited_rk_enums)
        isCompletable :: [RankEnum] -> Bool
        isCompletable suited_slide = any checkRankEnum rank_enums
          where
            checkRankEnum rk_enum = any (isStraight . uniqSorted) completeSlide
              where
                completeSlide = case rk_enum of
                    13 -> [0 : suited_slide, 13 : suited_slide]
                    _ -> [rk_enum : suited_slide]

    isStraight :: [RankEnum] -> Bool
    isStraight rk_enums = length rk_enums >= fs_sz && all check diffs
      where
        diffs = zipWith (-) (tail rk_enums) rk_enums
        check diff = diff >= 1 && diff <= dist

genBySuit :: forall a. (Enum a) => (Card -> a) -> Hand -> Vector [a]
genBySuit transform cards = runST $ do
    res <- MV.replicate 4 []
    forM_ cards $ \card -> do
        let idx = (fromEnum . suit) card
        old <- MV.read res idx
        MV.write res idx (transform card : old)
    V.freeze res

genCardsBySuit :: Hand -> Vector [Card]
genCardsBySuit = genBySuit id

genCounter :: forall a. (Enum a) => (Card -> a) -> Sz -> Hand -> Vector Int
genCounter transform sz cards = runST $ do
    vec <- MV.replicate sz 0
    forM_ cards $ \card -> do
        let idx = (fromEnum . transform) card
        MV.modify vec (+ quantity card) idx
    V.freeze vec

genCounterSuit :: Hand -> Counter
genCounterSuit = genCounter suit 4

genCounterRank :: Hand -> Counter
genCounterRank = genCounter rank 14

uniqSorted :: forall a. (Ord a) => [a] -> [a]
uniqSorted = S.toList . S.fromList

categorizeIndices :: Vector Int -> CntStats
categorizeIndices cnter =
    let
        step (cnt1, cnt2, cnt3, cnt4, cntm5) card_idx
            | val == 1 = (card_idx : cnt1, cnt2, cnt3, cnt4, cntm5)
            | val == 2 = (cnt1, card_idx : cnt2, cnt3, cnt4, cntm5)
            | val == 3 = (cnt1, cnt2, card_idx : cnt3, cnt4, cntm5)
            | val == 4 = (cnt1, cnt2, cnt3, card_idx : cnt4, cntm5)
            | val >= 5 = (cnt1, cnt2, cnt3, cnt4, card_idx : cntm5)
            | otherwise = (cnt1, cnt2, cnt3, cnt4, cntm5)
          where
            val = cnter V.! card_idx
     in
        foldl' step ([], [], [], [], []) [0 .. (V.length cnter - 1)]

genSlides :: Sz -> [RankEnum] -> [[RankEnum]]
genSlides slide_sz rank_enums = genAceSlide : genNormalSlide Seq.empty rank_enums
  where
    genNormalSlide :: Seq RankEnum -> [RankEnum] -> [[RankEnum]]
    genNormalSlide _ [] = []
    genNormalSlide slide (y : ys)
        | Seq.length slide < slide_sz - 1 = genNormalSlide (slide |> y) ys
        | otherwise =
            let new_slide = slide |> y
             in toList new_slide : genNormalSlide (Seq.drop 1 new_slide) ys
    genAceSlide = if last rank_enums == 13 then 0 : take (slide_sz - 1) rank_enums else []
