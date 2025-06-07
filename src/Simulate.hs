{-# LANGUAGE TupleSections #-}

module Simulate where

import Cards (Card (NormalCard, StoneCard, edition, enhancement, seal), Deck, Edition, Enhancement, Seal)
import Control.Concurrent.Async (replicateConcurrently)
import Control.Monad (forM_, when)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import HandType (Dist, Hand, HandType (..), Sz, getHandType)
import System.Random (randomRIO)

type SubHand = [Card]

shuffle :: [Card] -> IO [Card]
shuffle xs = do
    let n = length xs
    mvec <- V.thaw (V.fromList xs) -- mutable vector
    forM_ [0 .. n - 2] $ \i -> do
        j <- randomRIO (i, n - 1)
        MV.swap mvec i j
    V.toList <$> V.freeze mvec

simulate ::
    Deck ->
    Hand ->
    Sz ->
    Dist ->
    Int ->
    Bool ->
    IO (Map Enhancement Int, Map Edition Int, Map (Maybe Seal) Int, Int, HandType)
simulate deck hand fs_sz dist num_draw is_logged = do
    shuffled <- shuffle deck
    let drawn = take num_draw shuffled
        newHand = hand ++ drawn
        enhancements = [e | NormalCard{enhancement = e} <- newHand]
        editions = [e | NormalCard{edition = e} <- newHand] ++ [e | StoneCard{edition = e} <- newHand]
        seals =
            [s | NormalCard{seal = s} <- newHand] ++ [s | StoneCard{seal = s} <- newHand]
        stones = length [() | StoneCard{} <- newHand]
        enhancementStats = countElems enhancements
        editionStats = countElems editions
        sealStats = countElems (map Just seals)
        handType = getHandType newHand fs_sz dist
    when is_logged $ do
        putStrLn $ "Enhancement stats: " ++ show enhancementStats
        putStrLn $ "Edition stats: " ++ show editionStats
        putStrLn $ "Seal stats: " ++ show sealStats
        putStrLn $ "Stone count: " ++ show stones
        putStrLn $ "Hand type: " ++ show handType
    return (enhancementStats, editionStats, sealStats, stones, handType)

simulateMul ::
    Deck ->
    Hand ->
    Sz ->
    Dist ->
    Int ->
    Int ->
    Bool ->
    IO (Map Enhancement Double, Map Edition Double, Map (Maybe Seal) Double, Double, Map HandType Int)
simulateMul deck hand fs_sz dist num_sim num_draw is_logged = do
    results <- replicateConcurrently num_sim (simulate deck hand fs_sz dist num_draw is_logged)
    let resultsVec = V.fromList results
        (enhLists, edLists, sealLists, stoneCounts, handTypes) = V.unzip5 resultsVec
        avgEnh = averageMaps (V.toList enhLists) num_sim
        avgEd = averageMaps (V.toList edLists) num_sim
        avgSeal = averageMaps (V.toList sealLists) num_sim
        avgStones = fromIntegral (sum stoneCounts) / fromIntegral num_sim
        handTypeDist = countElems (V.toList handTypes)
    return (avgEnh, avgEd, avgSeal, avgStones, handTypeDist)

countElems :: (Ord a) => [a] -> Map a Int
countElems = Map.fromListWith (+) . map (,1)

averageMaps :: (Ord k, Fractional v) => [Map k Int] -> Int -> Map k v
averageMaps maps total =
    Map.map (/ fromIntegral total) $
        Map.unionsWith (+) $
            map (Map.map fromIntegral) maps
