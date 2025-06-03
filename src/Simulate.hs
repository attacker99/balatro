module Simulate where

import Cards (Card (..))
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (replicateM, when)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import HandType (HandType (..), getHandType)
import System.Random (randomRIO)
import CardParser (stringToCard)

type Deck = V.Vector Card
type Hand = V.Vector Card
type Sz = Int
type Dist = Int

parseCardsWithQuantity :: String -> V.Vector (Maybe Card)
parseCardsWithQuantity input =
    let specs = words input
        parseSpec spec = stringToCard spec
     in V.fromList $ map parseSpec specs

simulate :: Deck -> Deck -> Deck -> Sz -> Sz -> Dist -> Bool -> IO (HandType, Deck)
simulate deck hand discard drawNum fsSz dist enableLogging = do
    when enableLogging $ do
        putStrLn "Simulating with:"
        putStrLn $ "  deck: " ++ show (V.toList deck)
        putStrLn $ "  hand: " ++ show (V.toList hand)
        putStrLn $ "  discard: " ++ show (V.toList discard)
        putStrLn $ "  drawNum: " ++ show drawNum
        putStrLn $ "  fsSz: " ++ show fsSz
        putStrLn $ "  dist: " ++ show dist

    let remainingHand = V.filter (\card -> card `notElem` V.toList discard) hand
    when enableLogging $ putStrLn $ "  remainingHand: " ++ show (V.toList remainingHand)

    let availableCards = V.filter (\card -> card `notElem` V.toList hand) deck
    when enableLogging $ putStrLn $ "  availableCards: " ++ show (V.toList availableCards)

    shuffledCards <- shuffle availableCards
    when enableLogging $ putStrLn $ "  shuffledCards: " ++ show (V.toList shuffledCards)

    let drawnCards = V.take drawNum shuffledCards
    when enableLogging $ putStrLn $ "  drawnCards: " ++ show (V.toList drawnCards)

    let finalCards = V.concat [drawnCards, remainingHand]
    when enableLogging $ putStrLn $ "  finalCards: " ++ show (V.toList finalCards)

    let handType = getHandType (V.toList finalCards) fsSz dist
    when enableLogging $ putStrLn $ "  handType: " ++ show handType
    return (handType, finalCards)

shuffle :: Deck -> IO Deck
shuffle xs = do
    let n = V.length xs
    indices <- replicateM n (randomRIO (0, n - 1))
    return $ V.map (xs V.!) (V.fromList indices)

simulateMul :: Deck -> Hand -> Deck -> Sz -> Sz -> Dist -> Int -> Bool -> IO [(HandType, Double)]
simulateMul deck hand discard drawNum fsSz dist numSims enableLogging = do
    results <- mapConcurrently (\_ -> fst <$> simulate deck hand discard drawNum fsSz dist enableLogging) [1 .. numSims]
    let counts = Map.fromListWith (+) [(htype, 1 :: Int) | htype <- results]
        total = fromIntegral numSims
    return $ map (\(ht, count) -> (ht, (fromIntegral count / total) * 100)) (Map.toList counts)

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
    [(x, "")] -> Just x
    _ -> Nothing