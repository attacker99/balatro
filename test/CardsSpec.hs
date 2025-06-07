module CardsSpec where

import Cards
import qualified Data.Vector as V
import Test.Hspec

spec :: Spec
spec = do
    describe "Card equality" $ do
        it "treats identical base cards as equal" $ do
            let c1 = mkBaseCard Ace Spade
            let c2 = mkBaseCard Ace Spade
            c1 `shouldBe` c2

        it "wild cards match rank only" $ do
            let c1 = (mkBaseCard Ace Spade){enhancement = Wild}
            let c2 = (mkBaseCard Ace Heart)
            c1 `shouldBe` c2

        it "stone cards only equal to other stone cards" $ do
            let c1 = StoneCard 1 Base NoneS
            let c2 = (mkBaseCard Ace Spade)
            c1 `shouldNotBe` c2
            c1 `shouldBe` c1

    describe "Deck contents" $ do
        it "ranks contains 13 ranks" $ do
            length allRanks `shouldBe` 13

        it "suits contains 4 suits" $ do
            length allSuits `shouldBe` 4

        it "stdDeck contains 52 cards" $ do
            length stdDeck `shouldBe` 52

        it "abandonedDeck contains 40 cards" $ do
            length abandonedDeck `shouldBe` (10 * 4)

        it "checkeredDeck contains 26 cards" $ do
            length checkeredDeck `shouldBe` (13 * 2)

    describe "Enhancement checkers" $ do
        let wildCard = (mkBaseCard Ten Heart){enhancement = Wild}
        let stoneCard = StoneCard 1 Base NoneS
        let plainCard = mkBaseCard Two Club

        it "isWild detects Wild enhancement" $ do
            isWild wildCard `shouldBe` True
            isWild stoneCard `shouldBe` False
            isWild plainCard `shouldBe` False

        it "isStone detects Stone enhancement" $ do
            isStone stoneCard `shouldBe` True
            isStone wildCard `shouldBe` False
            isStone plainCard `shouldBe` False