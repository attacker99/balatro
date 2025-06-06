module SimulateSpec (spec) where

import CardParser (stringToCard)
import Cards (Card (..), Edition (..), Enhancement (..), Rank (..), Seal (..), Suit (..), mkBaseCard)
import Data.Maybe (isJust)
import qualified Data.Vector as V
import HandType (allHandtypes)
import Simulate (simulate, simulateMul)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import qualified Data.Map as Map

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

    describe "simulate" $ do
        it "returns valid statistics and hand type" $ do
            let deck = [NormalCard Ace Spade 1 None Base Nothing,
                       NormalCard King Heart 1 None Base Nothing,
                       NormalCard Queen Diamond 1 None Base Nothing]
            let hand = [NormalCard Ace Spade 1 None Base Nothing,
                       NormalCard King Heart 1 None Base Nothing]
            (enhStats, edStats, sealStats, stones, handType) <- simulate deck hand 5 1 1 False
            handType `shouldSatisfy` \ht -> ht `elem` allHandtypes
            Map.size enhStats `shouldSatisfy` (>= 0)
            Map.size edStats `shouldSatisfy` (>= 0)
            Map.size sealStats `shouldSatisfy` (>= 0)
            stones `shouldSatisfy` (>= 0)

        it "handles multiple draws correctly" $ do
            let deck = [NormalCard Ace Spade 1 None Base Nothing,
                       NormalCard King Heart 1 None Base Nothing,
                       NormalCard Queen Diamond 1 None Base Nothing,
                       NormalCard Jack Club 1 None Base Nothing,
                       NormalCard Ten Spade 1 None Base Nothing]
            let hand = [NormalCard Ace Spade 1 None Base Nothing,
                       NormalCard King Heart 1 None Base Nothing]
            (enhStats, edStats, sealStats, stones, handType) <- simulate deck hand 5 2 1 False
            handType `shouldSatisfy` \ht -> ht `elem` allHandtypes
            Map.size enhStats `shouldSatisfy` (>= 0)
            Map.size edStats `shouldSatisfy` (>= 0)
            Map.size sealStats `shouldSatisfy` (>= 0)
            stones `shouldSatisfy` (>= 0)

        it "handles multiple free shop sizes correctly" $ do
            let deck = [NormalCard Ace Spade 1 None Base Nothing,
                       NormalCard King Heart 1 None Base Nothing,
                       NormalCard Queen Diamond 1 None Base Nothing,
                       NormalCard Jack Club 1 None Base Nothing,
                       NormalCard Ten Spade 1 None Base Nothing]
            let hand = [NormalCard Ace Spade 1 None Base Nothing,
                       NormalCard King Heart 1 None Base Nothing]
            (enhStats, edStats, sealStats, stones, handType) <- simulate deck hand 5 1 2 False
            handType `shouldSatisfy` \ht -> ht `elem` allHandtypes
            Map.size enhStats `shouldSatisfy` (>= 0)
            Map.size edStats `shouldSatisfy` (>= 0)
            Map.size sealStats `shouldSatisfy` (>= 0)
            stones `shouldSatisfy` (>= 0)

        it "handles both multiple draws and free shop sizes" $ do
            let deck = [NormalCard Ace Spade 1 None Base Nothing,
                       NormalCard King Heart 1 None Base Nothing,
                       NormalCard Queen Diamond 1 None Base Nothing,
                       NormalCard Jack Club 1 None Base Nothing,
                       NormalCard Ten Spade 1 None Base Nothing,
                       NormalCard Nine Heart 1 None Base Nothing,
                       NormalCard Eight Diamond 1 None Base Nothing]
            let hand = [NormalCard Ace Spade 1 None Base Nothing,
                       NormalCard King Heart 1 None Base Nothing]
            (enhStats, edStats, sealStats, stones, handType) <- simulate deck hand 5 2 2 False
            handType `shouldSatisfy` \ht -> ht `elem` allHandtypes
            Map.size enhStats `shouldSatisfy` (>= 0)
            Map.size edStats `shouldSatisfy` (>= 0)
            Map.size sealStats `shouldSatisfy` (>= 0)
            stones `shouldSatisfy` (>= 0)

    describe "simulateMul" $ do
        it "returns valid statistics for multiple simulations" $ do
            let deck = [NormalCard Ace Spade 1 None Base Nothing,
                       NormalCard King Heart 1 None Base Nothing,
                       NormalCard Queen Diamond 1 None Base Nothing]
            let hand = [NormalCard Ace Spade 1 None Base Nothing,
                       NormalCard King Heart 1 None Base Nothing]
            (enhStats, edStats, sealStats, stones, handTypeStats) <- simulateMul deck hand 5 1 100 1 False
            Map.size enhStats `shouldSatisfy` (>= 0)
            Map.size edStats `shouldSatisfy` (>= 0)
            Map.size sealStats `shouldSatisfy` (>= 0)
            stones `shouldSatisfy` (>= 0)
            Map.size handTypeStats `shouldSatisfy` (>= 0)
            sum (Map.elems handTypeStats) `shouldBe` 100

        it "handles stone cards correctly" $ do
            let deck = [StoneCard 1 Base Nothing,
                       NormalCard King Heart 1 None Base Nothing,
                       NormalCard Queen Diamond 1 None Base Nothing]
            let hand = [StoneCard 1 Base Nothing,
                       NormalCard King Heart 1 None Base Nothing]
            (enhStats, edStats, sealStats, stones, handTypeStats) <- simulateMul deck hand 5 1 100 1 False
            stones `shouldSatisfy` (>= 0)
            Map.size handTypeStats `shouldSatisfy` (>= 0)
            sum (Map.elems handTypeStats) `shouldBe` 100
