{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Aeson (object, withObject, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower, toUpper)
import qualified Data.HashMap.Strict ()
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Vector as V
import GHC.Generics
import Network.HTTP.Types (status400)
import Network.Wai
import Network.Wai.Middleware.Cors
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Web.Scotty

import Cards
import HandType (HandType (..), getHandType)
import Simulate (parseCardsWithQuantity, simulate, simulateMul)

data EvaluateRequest = EvaluateRequest
    { cards :: [Card]
    , size :: Int
    , distance :: Int
    }
    deriving (Show, Generic)

data SimulateRequest = SimulateRequest
    { deck :: String
    , hand :: String
    , discard :: String
    , drawNum :: Int
    , fsSz :: Int
    , dist :: Int
    , numSims :: Int
    , enableLogging :: Bool
    }
    deriving (Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

data SimulateResponse = SimulateResponse
    { handType :: HandType
    , drawnCards :: [Card]
    , finalHand :: [Card]
    }
    deriving (Show, Generic, Aeson.ToJSON)

newtype SimulateMulResponse = SimulateMulResponse
    { percentages :: [(String, Double)]
    }
    deriving (Show, Generic, Aeson.ToJSON)

instance Aeson.ToJSON EvaluateRequest where
    toJSON (EvaluateRequest cards size distance) =
        Aeson.object
            [ "cards" Aeson..= cards
            , "size" Aeson..= size
            , "distance" Aeson..= distance
            ]

instance Aeson.FromJSON EvaluateRequest where
    parseJSON = withObject "EvaluateRequest" $ \v -> do
        cardsList <- v .: "cards"
        sizeVal <- v .: "size"
        distVal <- v .: "distance"
        cards <- mapM parseCard cardsList
        return $ EvaluateRequest cards sizeVal distVal
      where
        parseCard = withObject "Card" $ \v -> do
            rankStr <- v .: "rank"
            suitStr <- v .: "suit"
            enhancementStr <- v .:? "enhancement" .!= ("None" :: String)
            editionStr <- v .:? "edition" .!= ("Base" :: String)
            sealStr <- v .:? "seal" :: Parser (Maybe String)
            let parseRank s = case map toUpper s of
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
                parseSuit s = case map toLower s of
                    "s" -> Just Spade
                    "h" -> Just Heart
                    "d" -> Just Diamond
                    "c" -> Just Club
                    _ -> Nothing
                parseEnhancement s = case map toLower s of
                    "none" -> None
                    "nonus" -> Bonus
                    "mult" -> Mult
                    "wild" -> Wild
                    "glass" -> Glass
                    "steel" -> Steel
                    "gold" -> Gold
                    "lucky" -> Lucky
                    _ -> None
                parseEdition s = case map toLower s of
                    "base" -> Base
                    "foil" -> Foil
                    "holographic" -> Holographic
                    "polychrome" -> Polychrome
                    _ -> Base
                parseSeal ms = case ms of
                    Just "Gold" -> Just GoldS
                    Just "Red" -> Just Red
                    Just "Blue" -> Just Blue
                    Just "Purple" -> Just Purple
                    _ -> Nothing
            if map toUpper rankStr == "S" && map toUpper suitStr == "S"
                then return $ StoneCard 1 (parseEdition editionStr) (parseSeal sealStr)
                else case (parseRank rankStr, parseSuit suitStr) of
                    (Just r, Just s) -> return $ NormalCard r s 1 (parseEnhancement enhancementStr) (parseEdition editionStr) (parseSeal sealStr)
                    _ -> fail $ "Invalid rank or suit: " ++ rankStr ++ " " ++ suitStr

-- Helper functions to convert string ranks and suits to their enum values
stringToRank :: String -> Maybe Rank
stringToRank s = case s of
    "A" -> Just Ace
    "2" -> Just Two
    "3" -> Just Three
    "4" -> Just Four
    "5" -> Just Five
    "6" -> Just Six
    "7" -> Just Seven
    "8" -> Just Eight
    "9" -> Just Nine
    "10" -> Just Ten
    "J" -> Just Jack
    "Q" -> Just Queen
    "K" -> Just King
    "T" -> Just Ten -- Add support for 'T' as Ten
    _ -> Nothing

stringToSuit :: String -> Maybe Suit
stringToSuit s = case s of
    "s" -> Just Spade
    "h" -> Just Heart
    "d" -> Just Diamond
    "c" -> Just Club
    _ -> Nothing

main :: IO ()
main = do
    createDirectoryIfMissing True "static"

    scotty 3000 $ do
        -- Add CORS middleware
        middleware $
            cors $
                const $
                    Just
                        simpleCorsResourcePolicy
                            { corsRequestHeaders = ["Content-Type"]
                            , corsMethods = ["GET", "POST", "OPTIONS"]
                            }

        -- Add logging middleware
        middleware $ \app req respond -> do
            putStrLn $ "Received request: " ++ show (requestMethod req) ++ " " ++ show (rawPathInfo req)
            app req respond

        -- Serve static files
        get "/static/:file" $ do
            fileName <- pathParam "file"
            liftIO $ putStrLn $ "Serving static file: " ++ fileName
            let filePath = "static/" ++ fileName
            exists <- liftIO $ doesFileExist filePath
            if exists
                then Web.Scotty.file filePath
                else do
                    status status400
                    text "File not found"

        -- Evaluate hand endpoint
        post "/api/evaluate" $ do
            liftIO $ putStrLn "Received /api/evaluate request"
            req <- request
            requestBody <- liftIO $ getRequestBodyChunk req
            let rawBodyStr = show requestBody
            liftIO $ putStrLn $ "Raw request body: " ++ rawBodyStr
            case Aeson.decode (BL.fromStrict requestBody) of
                Nothing -> do
                    liftIO $ putStrLn $ "Failed to decode request body: " ++ rawBodyStr
                    json $ object ["error" .= ("Invalid request format" :: String)]
                Just reqData -> do
                    liftIO $ putStrLn $ "Successfully decoded request: " ++ show reqData
                    case cards reqData of
                        [] -> json $ object ["error" .= ("No cards provided" :: String)]
                        cs -> do
                            let cardsVec = V.fromList cs
                            liftIO $ putStrLn $ "Converted cards to Vector: " ++ show (V.toList cardsVec)
                            let handType = getHandType (V.toList cardsVec) (size reqData) (distance reqData)
                            liftIO $ putStrLn $ "Hand type result: " ++ show handType
                            json $ object ["handType" .= handType]

        -- Single simulation endpoint
        post "/api/simulate" $ do
            requestBody <- body
            case Aeson.decode requestBody of
                Just SimulateRequest{..} -> do
                    liftIO $ putStrLn $ "Received simulation request: " ++ show SimulateRequest{..}

                    let deckCards = parseCardsWithQuantity deck
                    liftIO $ putStrLn $ "Parsed deck cards: " ++ show (V.toList deckCards)

                    let handCards = parseCardsWithQuantity hand
                    liftIO $ putStrLn $ "Parsed hand cards: " ++ show (V.toList handCards)

                    let discardCards = parseCardsWithQuantity discard
                    liftIO $ putStrLn $ "Parsed discard cards: " ++ show (V.toList discardCards)

                    if V.null deckCards
                        then do
                            status status400
                            json $ object ["error" .= ("Invalid deck format" :: String)]
                        else
                            if V.null handCards
                                then do
                                    status status400
                                    json $ object ["error" .= ("Invalid hand format" :: String)]
                                else do
                                    (handType, finalCards) <- liftIO $ simulate deckCards handCards discardCards drawNum fsSz dist enableLogging
                                    liftIO $ putStrLn $ "Simulation result - handType: " ++ show handType ++ ", finalCards: " ++ show (V.toList finalCards)

                                    -- Extract drawn cards from final cards
                                    let drawnCards = V.take drawNum finalCards
                                    json $ SimulateResponse handType (V.toList drawnCards) (V.toList finalCards)
                Nothing -> do
                    status status400
                    json $ object ["error" .= ("Invalid request format" :: String)]

        -- Multiple simulation endpoint
        post "/api/simulate_mul" $ do
            requestBody <- body
            case Aeson.decode requestBody of
                Just SimulateRequest{..} -> do
                    let deckCards = parseCardsWithQuantity deck
                    let handCards = parseCardsWithQuantity hand
                    let discardCards = parseCardsWithQuantity discard

                    if V.null deckCards
                        then do
                            status status400
                            json $ Aeson.object ["error" Aeson..= ("Invalid deck format" :: String)]
                        else
                            if V.null handCards
                                then do
                                    status status400
                                    json $ Aeson.object ["error" Aeson..= ("Invalid hand format" :: String)]
                                else do
                                    percentages <- liftIO $ simulateMul deckCards handCards discardCards drawNum fsSz dist numSims enableLogging
                                    let sortedPercentages = sortBy (comparing snd) $ first show <$> percentages
                                    json $ SimulateMulResponse sortedPercentages
                Nothing -> do
                    status status400
                    json $ Aeson.object ["error" Aeson..= ("Invalid request format" :: String)]

        -- Default route
        get "/" $ do
            redirect "/static/index.html"

    putStrLn "Starting server on port 3000..."
