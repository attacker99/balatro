{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Aeson (object, withObject, (.:), (.=))
import qualified Data.Aeson as Aeson
import GHC.Generics
import Network.HTTP.Types (status400)
import Network.Wai
import Network.Wai.Middleware.Cors
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeExtension)
import Web.Scotty
import qualified Data.Map as Map
import CardParser (stringToCard)
import Cards
import HandType (HandType (..), getHandType)
import Simulate (simulateMul)

data EvaluateRequest = EvaluateRequest
    { card_str :: String -- space-separated card string
    , fs_sz :: Int
    , dist :: Int
    }
    deriving (Show, Generic)

instance Aeson.FromJSON EvaluateRequest where
    parseJSON = withObject "EvaluateRequest" $ \v ->
        EvaluateRequest
            <$> v .: "card_str"
            <*> v .: "fs_sz"
            <*> v .: "dist"

instance Aeson.ToJSON EvaluateRequest where
    toJSON (EvaluateRequest card_str fs_sz dist) =
        Aeson.object
            [ "card_str" Aeson..= card_str
            , "fs_sz" Aeson..= fs_sz
            , "dist" Aeson..= dist
            ]

data SimulateRequest = SimulateRequest
    { remain_deck :: String
    , remain_hand :: String
    , sz :: Int
    , distance :: Int
    , num_sim :: Int
    , num_draw :: Int
    , is_logged :: Bool
    }
    deriving (Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

data SimulateResponse = SimulateResponse
    { enhancementStats :: [(Enhancement, Double)]
    , editionStats :: [(Edition, Double)]
    , sealStats :: [(Maybe Seal, Double)]
    , stones :: Double
    , handTypeStats :: [(HandType, Int)]
    }
    deriving (Show, Generic, Aeson.ToJSON)

-- Helper function to parse card strings
parseCardString :: String -> ActionM [Card]
parseCardString cardStr = do
    case traverse stringToCard (words cardStr) of
        Nothing -> do
            liftIO $ putStrLn $ "Failed to parse card string: " ++ cardStr
            json $ object ["error" .= ("Failed to parse card string: " ++ cardStr)]
            fail "Invalid cards"
        Just cards -> return cards

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
                            { corsRequestHeaders = ["Content-Type", "Accept", "Origin", "Authorization"]
                            , corsMethods = ["GET", "POST", "OPTIONS"]
                            , corsOrigins = Nothing  -- Allow all origins
                            , corsMaxAge = Just 86400  -- Cache preflight for 24 hours
                            , corsExposedHeaders = Just ["Content-Type", "Accept", "Access-Control-Allow-Origin"]
                            }

        -- Add logging middleware
        middleware $ \app req respond -> do
            putStrLn $ "Received request: " ++ show (requestMethod req) ++ " " ++ show (rawPathInfo req)
            putStrLn $ "Request headers: " ++ show (requestHeaders req)
            app req respond

        -- Test endpoint
        get "/api/test" $ do
            liftIO $ putStrLn "Received /api/test request"
            json $ object ["status" .= ("ok" :: String)]

        -- Serve static files
        get "/static/:file" $ do
            fileName <- pathParam "file"
            liftIO $ putStrLn $ "Serving static file: " ++ fileName
            let filePath = "static/" ++ fileName
            exists <- liftIO $ doesFileExist filePath
            if exists
                then do
                    let mimeType = case takeExtension fileName of
                            ".js" -> "application/javascript"
                            ".css" -> "text/css"
                            ".html" -> "text/html"
                            _ -> "text/plain"
                    setHeader "Content-Type" mimeType
                    setHeader "Access-Control-Allow-Origin" "*"
                    setHeader "Access-Control-Allow-Methods" "GET, POST, OPTIONS"
                    setHeader "Access-Control-Allow-Headers" "Content-Type, Accept"
                    Web.Scotty.file filePath
                else do
                    status status400
                    text "File not found"

        -- Evaluate hand endpoint
        post "/api/evaluate" $ do
            liftIO $ putStrLn "Received /api/evaluate request"
            reqBody <- body
            liftIO $ putStrLn $ "Raw request body: " ++ show reqBody
            case Aeson.decode reqBody :: Maybe EvaluateRequest of
                Nothing -> do
                    liftIO $ putStrLn $ "Failed to decode request body: " ++ show reqBody
                    status status400
                    json $ object ["error" .= ("Invalid request format" :: String)]
                Just (EvaluateRequest cardStr fsSz dist) -> do
                    liftIO $ putStrLn $ "Decoded request: " ++ show (cardStr, fsSz, dist)
                    cards <- parseCardString cardStr
                    let result = getHandType cards fsSz dist
                    liftIO $ putStrLn $ "Computed hand type: " ++ show result
                    setHeader "Access-Control-Allow-Origin" "*"
                    setHeader "Access-Control-Allow-Methods" "GET, POST, OPTIONS"
                    setHeader "Access-Control-Allow-Headers" "Content-Type, Accept"
                    json $ object ["handType" .= result]

        -- Simulate endpoint
        post "/api/simulate" $ do
            liftIO $ putStrLn "Received /api/simulate request"
            reqBody <- body
            liftIO $ putStrLn $ "Raw request body: " ++ show reqBody
            case Aeson.decode reqBody :: Maybe SimulateRequest of
                Nothing -> do
                    liftIO $ putStrLn $ "Failed to decode request body: " ++ show reqBody
                    status status400
                    json $ object ["error" .= ("Invalid request format" :: String)]
                Just (SimulateRequest remain_deck remain_hand fs_sz dist num_sim num_draw is_logged) -> do
                    liftIO $ putStrLn $ "Decoded request: " ++ show (remain_deck, remain_hand, fs_sz, dist, num_sim, num_draw, is_logged)
                    deck <- parseCardString remain_deck
                    liftIO $ putStrLn $ "Parsed deck cards: " ++ show deck
                    hand <- parseCardString remain_hand
                    liftIO $ putStrLn $ "Parsed hand cards: " ++ show hand
                    liftIO $ putStrLn $ "Starting simulation with " ++ show num_sim ++ " iterations..."
                    result <- liftIO $ simulateMul deck hand fs_sz dist num_sim num_draw is_logged
                    liftIO $ putStrLn $ "Simulation completed. Result: " ++ show result
                    let (enhStats, edStats, sealStats, stones, handTypeStats) = result
                    let response = SimulateResponse
                            { enhancementStats = Map.toList enhStats
                            , editionStats = Map.toList edStats
                            , sealStats = Map.toList sealStats
                            , stones = stones
                            , handTypeStats = Map.toList handTypeStats
                            }
                    liftIO $ putStrLn $ "Sending response: " ++ show response
                    setHeader "Access-Control-Allow-Origin" "*"
                    setHeader "Access-Control-Allow-Methods" "GET, POST, OPTIONS"
                    setHeader "Access-Control-Allow-Headers" "Content-Type, Accept"
                    json response

        get "/" $ do
            redirect "/static/index.html"

    putStrLn "Starting server on port 3000..."
