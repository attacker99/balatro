module HandTypeTest where

import Test.Hspec
import HandType (getHandType, HandType(..))
import Cards

spec :: Spec
spec = do
    describe "Basic Hand Types" $ do
        it "detects high card" $ do
            getHandType [mkBaseCard Two Heart, mkBaseCard Three Diamond, mkBaseCard Four Club, mkBaseCard Five Spade, mkBaseCard Seven Heart] 5 1 `shouldBe` HighCard

        it "detects pair" $ do
            getHandType [mkBaseCard Two Heart, mkBaseCard Two Diamond, mkBaseCard Three Club, mkBaseCard Four Spade, mkBaseCard Five Heart] 5 1 `shouldBe` Pair

        it "detects two pair" $ do
            getHandType [mkBaseCard Two Heart, mkBaseCard Two Diamond, mkBaseCard Three Club, mkBaseCard Three Spade, mkBaseCard Five Heart] 5 1 `shouldBe` TwoPair

        it "detects three of a kind" $ do
            getHandType [mkBaseCard Two Heart, mkBaseCard Two Diamond, mkBaseCard Two Club, mkBaseCard Four Spade, mkBaseCard Five Heart] 5 1 `shouldBe` ThreeKind

        it "detects four of a kind" $ do
            getHandType [mkBaseCard Two Heart, mkBaseCard Two Diamond, mkBaseCard Two Club, mkBaseCard Two Spade, mkBaseCard Three Heart] 5 1 `shouldBe` FourKind

        it "detects five of a kind" $ do
            getHandType [mkBaseCard Two Heart, mkBaseCard Two Diamond, mkBaseCard Two Club, mkBaseCard Two Spade, mkBaseCard Two Heart] 5 1 `shouldBe` FiveKind

        it "detects full house" $ do
            getHandType [mkBaseCard Two Heart, mkBaseCard Two Diamond, mkBaseCard Two Club, mkBaseCard Three Spade, mkBaseCard Three Heart] 5 1 `shouldBe` FullHouse

    describe "Straight Hands" $ do
        it "detects regular straight" $ do
            getHandType [mkBaseCard Two Heart, mkBaseCard Three Diamond, mkBaseCard Four Club, mkBaseCard Five Spade, mkBaseCard Six Heart] 5 1 `shouldBe` Straight

        it "detects ace-high straight" $ do
            getHandType [mkBaseCard Ten Heart, mkBaseCard Jack Diamond, mkBaseCard Queen Club, mkBaseCard King Spade, mkBaseCard Ace Heart] 5 1 `shouldBe` Straight

        it "detects ace-low straight" $ do
            getHandType [mkBaseCard Ace Heart, mkBaseCard Two Diamond, mkBaseCard Three Club, mkBaseCard Four Spade, mkBaseCard Five Heart] 5 1 `shouldBe` Straight

        it "detects straight with unsorted cards" $ do
            getHandType [mkBaseCard Four Heart, mkBaseCard Two Diamond, mkBaseCard Five Club, mkBaseCard Three Spade, mkBaseCard Six Heart] 5 1 `shouldBe` Straight

        it "detects straight with distance 2" $ do
            getHandType [mkBaseCard Two Heart, mkBaseCard Four Diamond, mkBaseCard Six Club, mkBaseCard Eight Spade, mkBaseCard Ten Heart] 5 2 `shouldBe` Straight

        it "detects straight with mixed distances" $ do
            getHandType [mkBaseCard Two Heart, mkBaseCard Three Diamond, mkBaseCard Five Club, mkBaseCard Six Spade, mkBaseCard Eight Heart] 5 2 `shouldBe` Straight

    describe "Flush Hands" $ do
        it "detects regular flush" $ do
            getHandType [mkBaseCard Two Heart, mkBaseCard Four Heart, mkBaseCard Seven Heart, mkBaseCard Nine Heart, mkBaseCard Queen Heart] 5 1 `shouldBe` Flush

        it "detects flush five" $ do
            getHandType [mkBaseCard Two Heart, mkBaseCard Two Heart, mkBaseCard Two Heart, mkBaseCard Two Heart, mkBaseCard Two Heart] 5 1 `shouldBe` FlushFive

        it "detects flush house" $ do
            getHandType [mkBaseCard Two Heart, mkBaseCard Two Heart, mkBaseCard Two Heart, mkBaseCard Three Heart, mkBaseCard Three Heart] 5 1 `shouldBe` FlushHouse

    describe "Straight Flush Hands" $ do
        it "detects regular straight flush" $ do
            getHandType [mkBaseCard Two Heart, mkBaseCard Three Heart, mkBaseCard Four Heart, mkBaseCard Five Heart, mkBaseCard Six Heart] 5 1 `shouldBe` StraightFlush

        it "detects ace-high straight flush" $ do
            getHandType [mkBaseCard Ten Heart, mkBaseCard Jack Heart, mkBaseCard Queen Heart, mkBaseCard King Heart, mkBaseCard Ace Heart] 5 1 `shouldBe` StraightFlush

        it "detects ace-low straight flush" $ do
            getHandType [mkBaseCard Ace Heart, mkBaseCard Two Heart, mkBaseCard Three Heart, mkBaseCard Four Heart, mkBaseCard Five Heart] 5 1 `shouldBe` StraightFlush

        it "detects straight flush with unsorted cards" $ do
            getHandType [mkBaseCard Four Heart, mkBaseCard Two Heart, mkBaseCard Five Heart, mkBaseCard Three Heart, mkBaseCard Six Heart] 5 1 `shouldBe` StraightFlush

        it "detects straight flush with distance 2" $ do
            getHandType [mkBaseCard Two Heart, mkBaseCard Four Heart, mkBaseCard Six Heart, mkBaseCard Eight Heart, mkBaseCard Ten Heart] 5 2 `shouldBe` StraightFlush

        it "detects ace-low straight flush with distance 2" $ do
            getHandType [mkBaseCard Ace Heart, mkBaseCard Three Heart, mkBaseCard Five Heart, mkBaseCard Seven Heart, mkBaseCard Nine Heart] 5 2 `shouldBe` StraightFlush

    describe "4-Card Hands" $ do
        it "detects 4-card straight" $ do
            getHandType [mkBaseCard Two Heart, mkBaseCard Three Diamond, mkBaseCard Four Club, mkBaseCard Five Spade] 4 1 `shouldBe` Straight

        it "detects 4-card ace-high straight" $ do
            getHandType [mkBaseCard Jack Heart, mkBaseCard Queen Diamond, mkBaseCard King Club, mkBaseCard Ace Spade] 4 1 `shouldBe` Straight

        it "detects 4-card ace-low straight" $ do
            getHandType [mkBaseCard Ace Heart, mkBaseCard Two Diamond, mkBaseCard Three Club, mkBaseCard Four Spade] 4 1 `shouldBe` Straight

        it "detects 4-card flush" $ do
            getHandType [mkBaseCard Two Heart, mkBaseCard Four Heart, mkBaseCard Seven Heart, mkBaseCard Queen Heart] 4 1 `shouldBe` Flush

        it "detects 4-card straight flush" $ do
            let cards = [mkBaseCard King Spade, mkBaseCard Queen Spade, mkBaseCard Ace Spade, mkBaseCard Jack Heart, Card Four Spade None Base Nothing]
            getHandType cards 4 2 `shouldBe` StraightFlush

        it "detects 4-card straight flush with distance 2" $ do
            let cards = [mkBaseCard Two Spade, mkBaseCard Six Diamond, mkBaseCard Ten Diamond, mkBaseCard King Spade, mkBaseCard Ace Spade, mkBaseCard Jack Heart, Card Four Spade None Base Nothing]
            getHandType cards 4 2 `shouldBe` StraightFlush

        it "detects 4-card straight flush with distance 2 (unsorted)" $ do
            let cards = [mkBaseCard King Spade, mkBaseCard Ace Spade, mkBaseCard Jack Spade, mkBaseCard Queen Spade]
            getHandType cards 4 2 `shouldBe` StraightFlush

        it "detects 4-card ace-low straight flush with distance 2" $ do
            getHandType [mkBaseCard Ace Spade, mkBaseCard Three Spade, mkBaseCard Five Heart, mkBaseCard Four Spade, mkBaseCard Two Spade] 4 2 `shouldBe` StraightFlush

    describe "Special Cases" $ do
        describe "flush house" $ do
            it "detects flush house with fs_sz = 4" $ do
                let hand = [Card Ace Spade None Base Nothing, Card Ace Spade None Base Nothing, Card Ace Diamond None Base Nothing, Card King Spade None Base Nothing, Card King Spade None Base Nothing]
                getHandType hand 4 1 `shouldBe` FlushHouse

            it "detects flush house with wild cards" $ do
                let hand = [Card Ace Spade None Base Nothing, Card Ace Spade None Base Nothing, Card Ace Spade None Base Nothing, Card King Spade None Base Nothing, Card King Diamond Wild Base Nothing]
                getHandType hand 5 1 `shouldBe` FlushHouse

        describe "straight flush with wild cards" $ do
            it "detects 5-card straight flush with 1 wild" $ do
                let hand = [Card Six Spade None Base Nothing, Card Seven Spade None Base Nothing, Card Eight Spade None Base Nothing, Card Nine Spade None Base Nothing, Card Ten Diamond Wild Base Nothing]
                getHandType hand 5 1 `shouldBe` StraightFlush

            it "detects 4-card straight flush with 1 wild" $ do
                let hand = [Card Six Heart None Base Nothing, Card Seven Heart None Base Nothing, Card Eight Heart None Base Nothing, Card Nine Heart Wild Base Nothing]
                getHandType hand 4 1 `shouldBe` StraightFlush

            it "detects 5-card straight flush with 2 wild" $ do
                let hand = [Card Six Spade None Base Nothing, Card Seven Spade None Base Nothing, Card Eight Spade None Base Nothing, Card Nine Spade Wild Base Nothing, Card Ten Spade Wild Base Nothing]
                getHandType hand 5 2 `shouldBe` StraightFlush

            it "detects 4-card straight flush with 2 wild" $ do
                let hand = [Card Six Heart None Base Nothing, Card Seven Heart None Base Nothing, Card Eight Heart Wild Base Nothing, Card Nine Heart Wild Base Nothing]
                getHandType hand 4 2 `shouldBe` StraightFlush

        describe "normal straight/straight flush" $ do
            it "detects 5-card straight flush with 2 wild" $ do
                let hand = [Card Five Spade None Base Nothing, Card Seven Spade None Base Nothing, Card Eight Spade None Base Nothing, Card Ten Diamond Wild Base Nothing, Card Queen Diamond Wild Base Nothing]
                getHandType hand 5 2 `shouldBe` StraightFlush

            it "detects 4-card straight flush with 2 wild" $ do
                let hand = [Card Three Heart None Base Nothing, Card Nine Heart None Base Nothing, Card Five Heart Wild Base Nothing, Card Queen Spade Wild Base Nothing, Card Seven Spade None Base Nothing]
                getHandType hand 4 2 `shouldBe` StraightFlush

            it "detects 4-card ace-low straight flush with 2 wild" $ do
                let hand = [Card Three Heart None Base Nothing, Card Ace Heart None Base Nothing, Card Five Heart Wild Base Nothing, Card Queen Spade Wild Base Nothing, Card Seven Spade None Base Nothing]
                getHandType hand 4 2 `shouldBe` StraightFlush

            it "detects 5-card straight with 2 wild" $ do
                let hand = [Card Five Spade None Base Nothing, Card Seven Club None Base Nothing, Card Eight Spade None Base Nothing, Card Nine Spade Wild Base Nothing, Card Jack Diamond Wild Base Nothing]
                getHandType hand 5 2 `shouldBe` Straight

            it "detects 4-card straight with 2 wild" $ do
                let hand = [Card Eight Heart None Base Nothing, Card Ten Spade None Base Nothing, Card Queen Heart Wild Base Nothing, Card Ace Heart Wild Base Nothing]
                getHandType hand 4 2 `shouldBe` Straight