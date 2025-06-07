module CardParserSpec where

import Test.Hspec
import Cards (Card(..), Rank(..), Suit(..), Enhancement(..), Edition(..), Seal(..))
import CardParser (stringToCard)

spec :: Spec
spec = describe "CardParser" $ do
    describe "parseCard" $ do
        describe "normal cards" $ do
            it "parses basic card" $ do
                stringToCard "2h" `shouldBe` Just (NormalCard Two Heart 1 None Base NoneS)

            it "parses card with quantity" $ do
                stringToCard "2h3" `shouldBe` Just (NormalCard Two Heart 3 None Base NoneS)

            it "parses card with enhancement" $ do
                stringToCard "2hw" `shouldBe` Just (NormalCard Two Heart 1 Wild Base NoneS)

            it "parses card with edition" $ do
                stringToCard "2hf" `shouldBe` Just (NormalCard Two Heart 1 None Foil NoneS)

            it "parses card with seal" $ do
                stringToCard "2hr" `shouldBe` Just (NormalCard Two Heart 1 None Base Red)

            it "parses full card" $ do
                stringToCard "2h3wfy" `shouldBe` Just (NormalCard Two Heart 3 Wild Foil GoldS)

        describe "stone cards" $ do
            it "parses basic stone card" $ do
                stringToCard "s" `shouldBe` Just (StoneCard 1 Base NoneS)

            it "parses stone card with quantity" $ do
                stringToCard "s3" `shouldBe` Just (StoneCard 3 Base NoneS)

            it "parses stone card with edition" $ do
                stringToCard "sf" `shouldBe` Just (StoneCard 1 Foil NoneS)

            it "parses stone card with seal" $ do
                stringToCard "sr" `shouldBe` Just (StoneCard 1 Base Red)

            it "parses full stone card" $ do
                stringToCard "s3fy" `shouldBe` Just (StoneCard 3 Foil GoldS)

        describe "error cases" $ do
            it "fails on empty string" $ do
                stringToCard "" `shouldBe` Nothing
            it "fails on invalid rank" $ do
                stringToCard "1h" `shouldBe` Nothing
            it "fails on invalid suit" $ do
                stringToCard "2x" `shouldBe` Nothing
            it "fails on invalid enhancement" $ do
                stringToCard "2hx" `shouldBe` Nothing
            it "fails on invalid enhancement 2" $ do
                stringToCard "2hxf" `shouldBe` Nothing
            it "fails on invalid edition" $ do
                stringToCard "2hfx" `shouldBe` Nothing
            it "fails on invalid edition" $ do
                stringToCard "2hfxy" `shouldBe` Nothing
            it "fails on invalid seal" $ do
                stringToCard "2hfrx" `shouldBe` Nothing

 