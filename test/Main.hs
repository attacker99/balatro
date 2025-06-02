import CardsSpec
import HandTypeTest
import Test.Hspec

main :: IO ()
main = hspec $ do
    CardsSpec.spec
    HandTypeTest.spec
