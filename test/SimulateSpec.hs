{-# LANGUAGE OverloadedStrings #-}

module SimulateSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Vector as V
import Cards (Card(..), Rank(..), Suit(..), Enhancement(..), Edition(..), Seal(..), mkBaseCard)
import Simulate (parseCard, parseCardsWithQuantity, simulate, simulateMul)
import HandType (HandType(..), allHandtypes)

spec :: Spec
spec = do
    describe "parseCard" $ do
        it "parses valid card codes correctly" $ do
            parseCard "As" `shouldBe` Just (mkBaseCard Ace Spade)
            parseCard "Kh" `shouldBe` Just (mkBaseCard King Heart)
            parseCard "Qd" `shouldBe` Just (mkBaseCard Queen Diamond)
            parseCard "Jc" `shouldBe` Just (mkBaseCard Jack Club)
            parseCard "Td" `shouldBe` Just (mkBaseCard Ten Diamond)
            parseCard "9h" `shouldBe` Just (mkBaseCard Nine Heart)
            parseCard "8s" `shouldBe` Just (mkBaseCard Eight Spade)
            parseCard "7c" `shouldBe` Just (mkBaseCard Seven Club)
            parseCard "6d" `shouldBe` Just (mkBaseCard Six Diamond)
            parseCard "5h" `shouldBe` Just (mkBaseCard Five Heart)
            parseCard "4s" `shouldBe` Just (mkBaseCard Four Spade)
            parseCard "3c" `shouldBe` Just (mkBaseCard Three Club)
            parseCard "2d" `shouldBe` Just (mkBaseCard Two Diamond)

        it "parses cards with enhancements" $ do
            parseCard "Asb" `shouldBe` Just (Card Ace Spade Bonus Base Nothing)
            parseCard "Khm" `shouldBe` Just (Card King Heart Mult Base Nothing)
            parseCard "Qdw" `shouldBe` Just (Card Queen Diamond Wild Base Nothing)
            parseCard "Jcg" `shouldBe` Just (Card Jack Club Glass Base Nothing)
            parseCard "Tds" `shouldBe` Just (Card Ten Diamond Steel Base Nothing)
            parseCard "9hy" `shouldBe` Just (Card Nine Heart Gold Base Nothing)
            parseCard "8sl" `shouldBe` Just (Card Eight Spade Lucky Base Nothing)
            parseCard "7cn" `shouldBe` Just (Card Seven Club None Base Nothing)

        it "parses cards with editions" $ do
            parseCard "Asf" `shouldBe` Just (Card Ace Spade None Foil Nothing)
            parseCard "Khh" `shouldBe` Just (Card King Heart None Holographic Nothing)
            parseCard "Qdp" `shouldBe` Just (Card Queen Diamond None Polychrome Nothing)
            parseCard "Jcn" `shouldBe` Just (Card Jack Club None Base Nothing)

        it "parses cards with seals" $ do
            parseCard "Asy" `shouldBe` Just (Card Ace Spade None Base (Just GoldS))
            parseCard "Khr" `shouldBe` Just (Card King Heart None Base (Just Red))
            parseCard "Qdb" `shouldBe` Just (Card Queen Diamond None Base (Just Blue))
            parseCard "Jcp" `shouldBe` Just (Card Jack Club None Base (Just Purple))
            parseCard "Tdn" `shouldBe` Just (Card Ten Diamond None Base Nothing)

        it "parses cards with multiple properties" $ do
            parseCard "Asbfy" `shouldBe` Just (Card Ace Spade Bonus Foil (Just GoldS))
            parseCard "Khmhr" `shouldBe` Just (Card King Heart Mult Holographic (Just Red))
            parseCard "Qdwp" `shouldBe` Just (Card Queen Diamond Wild Polychrome Nothing)

        it "parses stone cards" $ do
            parseCard "s" `shouldBe` Just (Card Two Spade Stone Base Nothing)

        it "rejects invalid card codes" $ do
            parseCard "Xx" `shouldBe` Nothing
            parseCard "A" `shouldBe` Nothing
            parseCard "Asd" `shouldBe` Nothing
            parseCard "" `shouldBe` Nothing
            parseCard "Asx" `shouldBe` Nothing  -- Invalid enhancement
            parseCard "Asfx" `shouldBe` Nothing  -- Invalid edition
            parseCard "Asfyx" `shouldBe` Nothing  -- Invalid seal

    describe "parseCardsWithQuantity" $ do
        it "parses multiple cards correctly" $ do
            let result = parseCardsWithQuantity "As Kh Qd"
            V.length result `shouldBe` 3
            V.toList result `shouldBe` [mkBaseCard Ace Spade, mkBaseCard King Heart, mkBaseCard Queen Diamond]

        it "parses multiple cards with properties" $ do
            let result = parseCardsWithQuantity "Asbfy Khmhr Qdwp"
            V.length result `shouldBe` 3
            V.toList result `shouldBe`
                [ Card Ace Spade Bonus Foil (Just GoldS)
                , Card King Heart Mult Holographic (Just Red)
                , Card Queen Diamond Wild Polychrome Nothing
                ]

        it "handles empty input" $ do
            let result = parseCardsWithQuantity ""
            V.length result `shouldBe` 0

        it "handles invalid cards in input" $ do
            let result = parseCardsWithQuantity "As Xx Kh"
            V.length result `shouldBe` 2
            V.toList result `shouldBe` [mkBaseCard Ace Spade, mkBaseCard King Heart]

    describe "simulate" $ do
        it "returns a valid hand type" $ do
            let deck = V.fromList [mkBaseCard Ace Spade, mkBaseCard King Heart, mkBaseCard Queen Diamond]
            let hand = V.fromList [mkBaseCard Jack Club, mkBaseCard Ten Spade]
            let discard = V.fromList []
            (result, _) <- simulate deck hand discard 1 5 0 False
            result `shouldSatisfy` \ht -> ht `elem` allHandtypes

    describe "simulateMul" $ do
        it "returns percentages that sum to approximately 100" $ do
            let deck = V.fromList [mkBaseCard Ace Spade, mkBaseCard King Heart, mkBaseCard Queen Diamond]
            let hand = V.fromList [mkBaseCard Jack Club, mkBaseCard Ten Spade]
            let discard = V.fromList []
            results <- simulateMul deck hand discard 1 5 0 100 False
            let total = sum $ map snd results
            total `shouldSatisfy` \x -> abs (x - 100) < 1  -- Allow for small floating point errors

        it "simulates multiple hands correctly" $ do
            let deck = V.fromList [mkBaseCard Two Spade, mkBaseCard Three Heart]
            let hand = V.fromList [mkBaseCard Four Diamond]
            let discard = V.empty
            results <- simulateMul deck hand discard 1 1 1 100 False
            length results `shouldBe` 1 