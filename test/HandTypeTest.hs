module HandTypeTest where

import Cards
import HandType (HandType (..), getHandType)
import Test.Hspec

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

        it "detects three of a kind with quantity" $ do
            getHandType [NormalCard Two Spade 3 None Base Nothing] 5 1 `shouldBe` ThreeKind

        it "detects three of a kind wilds" $ do
            getHandType [NormalCard Two Spade 2 Wild Base Nothing, NormalCard Two Heart 1 None Base Nothing] 5 1 `shouldBe` ThreeKind

        it "detects four of a kind" $ do
            getHandType [mkBaseCard Two Heart, mkBaseCard Two Diamond, mkBaseCard Two Club, mkBaseCard Two Spade, mkBaseCard Three Heart] 5 1 `shouldBe` FourKind

        it "detects five of a kind" $ do
            getHandType [mkBaseCard Two Heart, mkBaseCard Two Diamond, mkBaseCard Two Club, mkBaseCard Two Spade, mkBaseCard Two Heart] 5 1 `shouldBe` FiveKind

        it "detects five of a kind with quantity" $ do
            getHandType [NormalCard Two Spade 5 None Base Nothing] 5 1 `shouldBe` FlushFive

        it "detects five of a kind with wilds" $ do
            getHandType [NormalCard Two Spade 3 Wild Base Nothing, NormalCard Two Heart 2 None Base Nothing] 5 1 `shouldBe` FlushFive

        it "detects five of a kind with mixed wilds and quantity" $ do
            getHandType [NormalCard Two Spade 3 None Base Nothing, NormalCard Two Heart 1 Wild Base Nothing, NormalCard Two Diamond 1 Wild Base Nothing] 5 1 `shouldBe` FlushFive

        it "detects four of a kind with quantity" $ do
            getHandType [NormalCard Two Spade 4 None Base Nothing, mkBaseCard Three Heart] 5 1 `shouldBe` FourKind

        it "detects four of a kind with wilds" $ do
            getHandType [NormalCard Two Spade 2 Wild Base Nothing, NormalCard Two Heart 2 None Base Nothing, mkBaseCard Three Heart] 5 1 `shouldBe` FourKind

        it "detects four of a kind with mixed wilds and quantity" $ do
            getHandType [NormalCard Two Spade 2 None Base Nothing, NormalCard Two Heart 1 Wild Base Nothing, NormalCard Two Diamond 1 Wild Base Nothing, mkBaseCard Three Heart] 5 1 `shouldBe` FourKind

        it "detects full house" $ do
            getHandType [mkBaseCard Two Heart, mkBaseCard Two Diamond, mkBaseCard Two Club, mkBaseCard Three Spade, mkBaseCard Three Heart] 5 1 `shouldBe` FullHouse

        it "detects full house" $ do
            getHandType [NormalCard Two Spade 2 None Base Nothing, NormalCard Three Heart 1 None Base Nothing, NormalCard Three Diamond 1 None Base Nothing, NormalCard Two Club 1 None Base Nothing] 5 1 `shouldBe` FullHouse

        it "detects flush five" $ do
            getHandType [mkBaseCard Two Heart, mkBaseCard Two Heart, mkBaseCard Two Heart, mkBaseCard Two Heart, mkBaseCard Two Heart] 5 1 `shouldBe` FlushFive

        it "detects flush five" $ do
            getHandType [NormalCard Two Heart 4 None Base Nothing, mkBaseCard Two Spade] 4 1 `shouldBe` FlushFive

        it "detects flush five with quantity" $ do
            getHandType [NormalCard Two Heart 5 None Base Nothing] 5 1 `shouldBe` FlushFive

        it "detects flush five with wilds" $ do
            getHandType [NormalCard Two Heart 3 Wild Base Nothing, NormalCard Two Heart 2 None Base Nothing] 5 1 `shouldBe` FlushFive

        it "detects flush five with mixed wilds and quantity" $ do
            getHandType [NormalCard Two Heart 3 None Base Nothing, NormalCard Two Heart 1 Wild Base Nothing, NormalCard Two Heart 1 Wild Base Nothing] 5 1 `shouldBe` FlushFive

        it "detects flush house" $ do
            getHandType [mkBaseCard Two Heart, mkBaseCard Two Heart, mkBaseCard Two Heart, mkBaseCard Three Heart, mkBaseCard Three Heart] 5 1 `shouldBe` FlushHouse

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
            let cards = [mkBaseCard King Spade, mkBaseCard Queen Spade, mkBaseCard Ace Spade, mkBaseCard Jack Heart, NormalCard Four Spade 1 None Base Nothing]
            getHandType cards 4 2 `shouldBe` StraightFlush

        it "detects 4-card straight flush with distance 2" $ do
            let cards = [mkBaseCard Two Spade, mkBaseCard Six Diamond, mkBaseCard Ten Diamond, mkBaseCard King Spade, mkBaseCard Ace Spade, mkBaseCard Jack Heart, NormalCard Four Spade 1 None Base Nothing]
            getHandType cards 4 2 `shouldBe` StraightFlush

        it "detects 4-card straight flush with distance 2 (unsorted)" $ do
            let cards = [mkBaseCard King Spade, mkBaseCard Ace Spade, mkBaseCard Jack Spade, mkBaseCard Queen Spade]
            getHandType cards 4 2 `shouldBe` StraightFlush

        it "detects 4-card ace-low straight flush with distance 2" $ do
            getHandType [mkBaseCard Ace Spade, mkBaseCard Three Spade, mkBaseCard Five Heart, mkBaseCard Four Spade, mkBaseCard Two Spade] 4 2 `shouldBe` StraightFlush

    describe "Special Cases" $ do
        describe "flush house" $ do
            it "detects flush house with fs_sz = 4" $ do
                let hand = [NormalCard Ace Spade 1 None Base Nothing, NormalCard Ace Spade 1 None Base Nothing, NormalCard Ace Diamond 1 None Base Nothing, NormalCard King Spade 1 None Base Nothing, NormalCard King Spade 1 None Base Nothing]
                getHandType hand 4 1 `shouldBe` FlushHouse

            it "detects flush house with fs_sz = 4" $ do
                let hand = [NormalCard Ace Spade 2 None Base Nothing, NormalCard King Spade 2 None Base Nothing, mkBaseCard Ace Diamond]
                getHandType hand 4 1 `shouldBe` FlushHouse

            it "detects flush house with 1 Wild cards" $ do
                let hand = [NormalCard Ace Spade 1 None Base Nothing, NormalCard Ace Spade 1 None Base Nothing, NormalCard Ace Spade 1 None Base Nothing, NormalCard King Spade 1 None Base Nothing, NormalCard King Diamond 1 Wild Base Nothing]
                getHandType hand 5 1 `shouldBe` FlushHouse

        describe "straight flush with 1 Wild NormalCards" $ do
            it "detects 5-NormalCard straight flush with 1 wild" $ do
                let hand = [NormalCard Six Spade 1 None Base Nothing, NormalCard Seven Spade 1 None Base Nothing, NormalCard Eight Spade 1 None Base Nothing, NormalCard Nine Spade 1 None Base Nothing, NormalCard Ten Diamond 1 Wild Base Nothing]
                getHandType hand 5 1 `shouldBe` StraightFlush

            it "detects 4-NormalCard straight flush with 1 wild" $ do
                let hand = [NormalCard Six Heart 1 None Base Nothing, NormalCard Seven Heart 1 None Base Nothing, NormalCard Eight Heart 1 None Base Nothing, NormalCard Nine Heart 1 Wild Base Nothing]
                getHandType hand 4 1 `shouldBe` StraightFlush

            it "detects 5-NormalCard straight flush with 2 wild" $ do
                let hand = [NormalCard Six Spade 1 None Base Nothing, NormalCard Seven Spade 1 None Base Nothing, NormalCard Eight Spade 1 None Base Nothing, NormalCard Nine Spade 1 Wild Base Nothing, NormalCard Ten Spade 1 Wild Base Nothing]
                getHandType hand 5 2 `shouldBe` StraightFlush

            it "detects 4-NormalCard straight flush with 2 wild" $ do
                let hand = [NormalCard Six Heart 1 None Base Nothing, NormalCard Seven Heart 1 None Base Nothing, NormalCard Eight Heart 1 Wild Base Nothing, NormalCard Nine Heart 1 Wild Base Nothing]
                getHandType hand 4 2 `shouldBe` StraightFlush

        describe "normal straight/straight flush" $ do
            it "detects 5-NormalCard straight flush with 2 wild" $ do
                let hand = [NormalCard Five Spade 1 None Base Nothing, NormalCard Seven Spade 1 None Base Nothing, NormalCard Eight Spade 1 None Base Nothing, NormalCard Ten Diamond 1 Wild Base Nothing, NormalCard Queen Diamond 1 Wild Base Nothing]
                getHandType hand 5 2 `shouldBe` StraightFlush

            it "detects 4-NormalCard straight flush with 2 wild" $ do
                let hand = [NormalCard Three Heart 1 None Base Nothing, NormalCard Nine Heart 1 None Base Nothing, NormalCard Five Heart 1 Wild Base Nothing, NormalCard Queen Spade 1 Wild Base Nothing, NormalCard Seven Spade 1 None Base Nothing]
                getHandType hand 4 2 `shouldBe` StraightFlush

            it "detects 4-NormalCard ace-low straight flush with 2 wild" $ do
                let hand = [NormalCard Three Heart 1 None Base Nothing, NormalCard Ace Heart 1 None Base Nothing, NormalCard Five Heart 1 Wild Base Nothing, NormalCard Queen Spade 1 Wild Base Nothing, NormalCard Seven Spade 1 None Base Nothing]
                getHandType hand 4 2 `shouldBe` StraightFlush

            it "detects 5-NormalCard straight with 2 wild" $ do
                let hand = [NormalCard Five Spade 1 None Base Nothing, NormalCard Seven Club 1 None Base Nothing, NormalCard Eight Spade 1 None Base Nothing, NormalCard Nine Spade 1 Wild Base Nothing, NormalCard Jack Diamond 1 Wild Base Nothing]
                getHandType hand 5 2 `shouldBe` Straight

            it "detects 4-NormalCard straight with 2 wild" $ do
                let hand = [NormalCard Eight Heart 1 None Base Nothing, NormalCard Ten Spade 1 None Base Nothing, NormalCard Queen Heart 1 Wild Base Nothing, NormalCard Ace Heart 1 Wild Base Nothing]
                getHandType hand 4 2 `shouldBe` Straight

        it "detects five of a kind with 3 non-wilds of one rank and 2 non-wilds of another (same suit)" $ do
            let hand = [NormalCard Ace Heart 3 None Base Nothing, NormalCard King Heart 2 None Base Nothing]
            getHandType hand 5 1 `shouldBe` FlushHouse

        it "detects five of a kind with 3 non-wilds of one rank and 2 non-wilds of another (same suit)" $ do
            let hand = [NormalCard Ace Heart 3 None Base Nothing, NormalCard King Heart 1 None Base Nothing, mkBaseCard King Diamond]
            getHandType hand 4 1 `shouldBe` FlushHouse

        it "detects flush house with 3 of one rank and 2 of another, all same suit" $ do
            let hand = [mkBaseCard Ace Heart, mkBaseCard Ace Heart, mkBaseCard Ace Heart, mkBaseCard King Heart, mkBaseCard King Heart]
            getHandType hand 5 1 `shouldBe` FlushHouse

        it "detects flush house with 3 of one rank and 2 of another, all same suit, different ranks" $ do
            let hand = [mkBaseCard Queen Spade, mkBaseCard Queen Spade, mkBaseCard Queen Spade, mkBaseCard Jack Spade, mkBaseCard Jack Spade]
            getHandType hand 5 1 `shouldBe` FlushHouse

        it "detects full house with 3 of one rank and 2 of another, mixed suits" $ do
            let hand = [mkBaseCard Ace Heart, mkBaseCard Ace Diamond, mkBaseCard Ace Club, mkBaseCard King Heart, mkBaseCard King Diamond]
            getHandType hand 5 1 `shouldBe` FullHouse

        it "detects full house with 3 of one rank and 2 of another, all different suits" $ do
            let hand = [mkBaseCard Ten Heart, mkBaseCard Ten Diamond, mkBaseCard Ten Club, mkBaseCard Nine Spade, mkBaseCard Nine Diamond]
            getHandType hand 5 1 `shouldBe` FullHouse

        it "detects five of a kind with 5 cards of the same rank" $ do
            let hand = [NormalCard Ace Heart 4 None Base Nothing, mkBaseCard Ace Diamond]
            getHandType hand 5 1 `shouldBe` FiveKind

        it "detects five of a kind with 3 cards of one rank and 2 cards of another rank" $ do
            let hand = [NormalCard Ace Heart 3 None Base Nothing, NormalCard King Heart 2 None Base Nothing]
            getHandType hand 5 1 `shouldBe` FlushHouse

        it "detects flush house with 3 cards of one rank and 2 cards of another rank, all same suit" $ do
            let hand = [NormalCard Ace Heart 3 None Base Nothing, NormalCard King Heart 2 None Base Nothing]
            getHandType hand 5 1 `shouldBe` FlushHouse

        it "detects full house with 3 cards of one rank and 2 cards of another rank, mixed suits" $ do
            let hand = [NormalCard Ace Heart 3 None Base Nothing, NormalCard King Diamond 2 None Base Nothing]
            getHandType hand 5 1 `shouldBe` FullHouse

        it "detects full house with 3 cards of one rank and 2 cards of another rank, all different suits" $ do
            let hand = [NormalCard Ten Heart 3 None Base Nothing, NormalCard Nine Diamond 2 None Base Nothing]
            getHandType hand 5 1 `shouldBe` FullHouse

        it "detects flush house with 3 cards of one rank and 2 cards of another rank" $ do
            let hand = [NormalCard Ace Heart 3 None Base Nothing, NormalCard King Heart 2 None Base Nothing]
            getHandType hand 5 1 `shouldBe` FlushHouse

        it "detects flush house with 3 of one rank and 2 of another, all same suit" $ do
            let hand = [mkBaseCard Ace Heart, mkBaseCard Ace Heart, mkBaseCard Ace Heart, mkBaseCard King Heart, mkBaseCard King Heart]
            getHandType hand 5 1 `shouldBe` FlushHouse
