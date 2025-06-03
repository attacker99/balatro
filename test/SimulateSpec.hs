module SimulateSpec (spec) where

import CardParser (stringToCard)
import Cards (Card (..), Edition (..), Enhancement (..), Rank (..), Seal (..), Suit (..), mkBaseCard)
import Data.Maybe (isJust)
import qualified Data.Vector as V
import HandType (allHandtypes)
import Simulate (parseCardsWithQuantity, simulate, simulateMul)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = do
    describe "parseCard" $ do
        it "parses valid card codes correctly" $ do
            stringToCard "As" `shouldBe` Just (mkBaseCard Ace Spade)
            stringToCard "Kh" `shouldBe` Just (mkBaseCard King Heart)
            stringToCard "Qd" `shouldBe` Just (mkBaseCard Queen Diamond)
            stringToCard "Jc" `shouldBe` Just (mkBaseCard Jack Club)
            stringToCard "Td" `shouldBe` Just (mkBaseCard Ten Diamond)
            stringToCard "9h" `shouldBe` Just (mkBaseCard Nine Heart)
            stringToCard "8s" `shouldBe` Just (mkBaseCard Eight Spade)
            stringToCard "7c" `shouldBe` Just (mkBaseCard Seven Club)
            stringToCard "6d" `shouldBe` Just (mkBaseCard Six Diamond)
            stringToCard "5h" `shouldBe` Just (mkBaseCard Five Heart)
            stringToCard "4s" `shouldBe` Just (mkBaseCard Four Spade)
            stringToCard "3c" `shouldBe` Just (mkBaseCard Three Club)
            stringToCard "2d" `shouldBe` Just (mkBaseCard Two Diamond)

        it "parses cards with enhancements" $ do
            stringToCard "Asb" `shouldBe` Just (NormalCard Ace Spade 1 Bonus Base Nothing)
            stringToCard "Khm" `shouldBe` Just (NormalCard King Heart 1 Mult Base Nothing)
            stringToCard "Qdw" `shouldBe` Just (NormalCard Queen Diamond 1 Wild Base Nothing)
            stringToCard "Jcg" `shouldBe` Just (NormalCard Jack Club 1 Glass Base Nothing)
            stringToCard "Tds" `shouldBe` Just (NormalCard Ten Diamond 1 Steel Base Nothing)
            stringToCard "9hy" `shouldBe` Just (NormalCard Nine Heart 1 Gold Base Nothing)
            stringToCard "8sl" `shouldBe` Just (NormalCard Eight Spade 1 Lucky Base Nothing)
            stringToCard "7cn" `shouldBe` Just (NormalCard Seven Club 1 None Base Nothing)

        it "parses cards with editions" $ do
            stringToCard "Asf" `shouldBe` Just (NormalCard Ace Spade 1 None Foil Nothing)
            stringToCard "Khh" `shouldBe` Just (NormalCard King Heart 1 None Holographic Nothing)
            stringToCard "Qdp" `shouldBe` Just (NormalCard Queen Diamond 1 None Polychrome Nothing)
            stringToCard "Jcn" `shouldBe` Just (NormalCard Jack Club 1 None Base Nothing)

        it "parses cards with seals" $ do
            stringToCard "Asy" `shouldBe` Just (NormalCard Ace Spade 1 None Base (Just GoldS))
            stringToCard "Khr" `shouldBe` Just (NormalCard King Heart 1 None Base (Just Red))
            stringToCard "Qdb" `shouldBe` Just (NormalCard Queen Diamond 1 None Base (Just Blue))
            stringToCard "Jcp" `shouldBe` Just (NormalCard Jack Club 1 None Base (Just Purple))
            stringToCard "Tdn" `shouldBe` Just (NormalCard Ten Diamond 1 None Base Nothing)

        it "parses cards with 1 Multiple properties" $ do
            stringToCard "Asbfy" `shouldBe` Just (NormalCard Ace Spade 1 Bonus Foil (Just GoldS))
            stringToCard "Khmhr" `shouldBe` Just (NormalCard King Heart 1 Mult Holographic (Just Red))
            stringToCard "Qdwp" `shouldBe` Just (NormalCard Queen Diamond 1 Wild Polychrome Nothing)

        it "parses stone cards" $ do
            stringToCard "s" `shouldBe` Just (StoneCard 1 Base Nothing)

        it "rejects invalid card codes" $ do
            stringToCard "Xx" `shouldBe` Nothing
            stringToCard "A" `shouldBe` Nothing
            stringToCard "Asd" `shouldBe` Nothing
            stringToCard "" `shouldBe` Nothing
            stringToCard "Asx" `shouldBe` Nothing -- Invalid enhancement
            stringToCard "Asfx" `shouldBe` Nothing -- Invalid edition
            stringToCard "Asfyx" `shouldBe` Nothing -- Invalid seal
    describe "parseCardsWithQuantity" $ do
        it "parses multiple cards correctly" $ do
            let result = parseCardsWithQuantity "As Kh Qd"
            V.length result `shouldBe` 3
            V.toList result `shouldBe` [Just (NormalCard Ace Spade 1 None Base Nothing), Just (NormalCard King Heart 1 None Base Nothing), Just (NormalCard Queen Diamond 1 None Base Nothing)]

        it "parses multiple cards with properties" $ do
            let result = parseCardsWithQuantity "Asbfy Khmhr Qdwp"
            V.length result `shouldBe` 3
            V.toList result
                `shouldBe` [ Just (NormalCard Ace Spade 1 Bonus Foil (Just GoldS))
                           , Just (NormalCard King Heart 1 Mult Holographic (Just Red))
                           , Just (NormalCard Queen Diamond 1 Wild Polychrome Nothing)
                           ]

        it "handles empty input" $ do
            let result = parseCardsWithQuantity ""
            V.length result `shouldBe` 0

        it "handles invalid cards in input" $ do
            let result = parseCardsWithQuantity "As Xx Kh"
            let validCards = V.filter (isJust) result
            V.length validCards `shouldBe` 2
            V.toList validCards `shouldBe` [Just (NormalCard Ace Spade 1 None Base Nothing), Just (NormalCard King Heart 1 None Base Nothing)]
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
            total `shouldSatisfy` \x -> abs (x - 100) < 1 -- Allow for small floating point errors
        it "simulates multiple hands correctly" $ do
            let deck = V.fromList [mkBaseCard Two Spade, mkBaseCard Three Heart]
            let hand = V.fromList [mkBaseCard Four Diamond]
            let discard = V.empty
            results <- simulateMul deck hand discard 1 1 1 100 False
            length results `shouldBe` 1
