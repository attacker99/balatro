{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simulate where

import qualified Data.Vector as V
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (replicateM, when)
import System.Random (randomRIO)
import HandType (HandType(..), getHandType)
import Cards (Card(..), Rank(..), Suit(..), Enhancement(..), Edition(..), Seal(..), mkBaseCard)
import qualified Data.Map.Strict as Map
import Data.Char (isDigit)

type Deck = V.Vector Card
type Hand = V.Vector Card
type Sz = Int
type Dist = Int

-- Convert string representation to Card
parseCard :: String -> Maybe Card
parseCard input =
    -- Handle stone cards
    if input `startsWith` "s" then do
        let (letters, digits) = break isDigit (tail input)
            quantity = if null digits then 1 else read digits
            ed | 'f' `elem` letters = Foil
               | 'h' `elem` letters = Holographic
               | 'p' `elem` letters = Polychrome
               | otherwise = Base
            sealType | 'y' `elem` letters = Just GoldS
                     | 'r' `elem` letters = Just Red
                     | 'b' `elem` letters = Just Blue
                     | 'p' `elem` letters = Just Purple
                     | otherwise = Nothing
        Just $ Card Two Spade Stone ed sealType
    else do
        -- Parse rank and suit
        let (rankPart, rest) = splitAt 1 input
        rankType <- case rankPart of
            "A" -> Just Ace
            "2" -> Just Two
            "3" -> Just Three
            "4" -> Just Four
            "5" -> Just Five
            "6" -> Just Six
            "7" -> Just Seven
            "8" -> Just Eight
            "9" -> Just Nine
            "T" -> Just Ten
            "J" -> Just Jack
            "Q" -> Just Queen
            "K" -> Just King
            _ -> Nothing
        let (suitPart, rest2) = splitAt 1 rest
        suitType <- case suitPart of
            "s" -> Just Spade
            "h" -> Just Heart
            "d" -> Just Diamond
            "c" -> Just Club
            _ -> Nothing
        -- Now rest2 is enhancement+edition+seal+quantity
        let (letters, digits) = break isDigit rest2
            quantity = if null digits then 1 else read digits
            enh | 'g' `elem` letters = Glass
                | 'y' `elem` letters = Gold
                | 'b' `elem` letters = Bonus
                | 'm' `elem` letters = Mult
                | 'w' `elem` letters = Wild
                | 's' `elem` letters = Steel
                | 'l' `elem` letters = Lucky
                | otherwise = None
            ed | 'f' `elem` letters = Foil
               | 'h' `elem` letters = Holographic
               | 'p' `elem` letters = Polychrome
               | otherwise = Base
            sealType | 'y' `elem` letters = Just GoldS
                     | 'r' `elem` letters = Just Red
                     | 'b' `elem` letters = Just Blue
                     | 'p' `elem` letters = Just Purple
                     | otherwise = Nothing
        -- Return the card (quantity is ignored here, handled elsewhere)
        Just $ Card rankType suitType enh ed sealType
  where
    startsWith :: String -> String -> Bool
    startsWith [] _ = True
    startsWith _ [] = False
    startsWith (x:xs) (y:ys) = x == y && startsWith xs ys

-- Convert string representation directly to Vector of cards
parseCardsWithQuantity :: String -> V.Vector Card
parseCardsWithQuantity input = 
    let specs = words input
        parseSpec spec = case parseCard spec of
            Just card -> [card]
            Nothing -> []
    in V.fromList $ concatMap parseSpec specs

-- Simulate drawing cards and finding the best hand
simulate :: Deck -> Deck -> Deck -> Sz -> Sz -> Dist -> Bool -> IO (HandType, Deck)
simulate deck hand discard drawNum fsSz dist enableLogging = do
    when enableLogging $ do
        putStrLn $ "Simulating with:"
        putStrLn $ "  deck: " ++ show (V.toList deck)
        putStrLn $ "  hand: " ++ show (V.toList hand)
        putStrLn $ "  discard: " ++ show (V.toList discard)
        putStrLn $ "  drawNum: " ++ show drawNum
        putStrLn $ "  fsSz: " ++ show fsSz
        putStrLn $ "  dist: " ++ show dist

    -- Filter out discarded cards from hand
    let remainingHand = V.filter (\card -> card `notElem` V.toList discard) hand
    when enableLogging $ putStrLn $ "  remainingHand: " ++ show (V.toList remainingHand)

    -- Get available cards from deck (excluding cards in hand)
    let availableCards = V.filter (\card -> card `notElem` V.toList hand) deck
    when enableLogging $ putStrLn $ "  availableCards: " ++ show (V.toList availableCards)

    -- Shuffle and draw cards
    shuffledCards <- shuffle availableCards
    when enableLogging $ putStrLn $ "  shuffledCards: " ++ show (V.toList shuffledCards)

    let drawnCards = V.take drawNum shuffledCards
    when enableLogging $ putStrLn $ "  drawnCards: " ++ show (V.toList drawnCards)

    -- Combine drawn cards with remaining hand
    let finalCards = V.concat [drawnCards, remainingHand]
    when enableLogging $ putStrLn $ "  finalCards: " ++ show (V.toList finalCards)

    -- Get hand type and return result
    let handType = getHandType (V.toList finalCards) fsSz dist
    when enableLogging $ putStrLn $ "  handType: " ++ show handType
    return (handType, finalCards)

-- Shuffle a Vector using Fisher-Yates algorithm
shuffle :: Deck -> IO Deck
shuffle xs = do
    let n = V.length xs
    indices <- replicateM n (randomRIO (0, n-1))
    return $ V.map (xs V.!) (V.fromList indices)

-- Run multiple simulations in parallel and collect results
simulateMul :: Deck -> Hand -> Deck -> Sz -> Sz -> Dist -> Int -> Bool -> IO [(HandType, Double)]
simulateMul deck hand discard drawNum fsSz dist numSims enableLogging = do
    results <- mapConcurrently (\_ -> fst <$> simulate deck hand discard drawNum fsSz dist enableLogging) [1..numSims]

    -- Count occurrences using Data.Map
    let counts = Map.fromListWith (+) [(htype, 1 :: Int) | htype <- results]
        total = fromIntegral numSims
    return $ map (\(ht, count) -> (ht, (fromIntegral count / total) * 100)) (Map.toList counts)

-- Helper function to read Maybe values
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(x, "")] -> Just x
    _ -> Nothing