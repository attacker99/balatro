module Main where

import Test.Hspec
import CardParserSpec
import CardsSpec
import HandTypeTest
import SimulateSpec

main :: IO ()
main = hspec $ do
    describe "CardParser" CardParserSpec.spec
    describe "Cards" CardsSpec.spec
    describe "HandType" HandTypeTest.spec
    describe "Simulate" SimulateSpec.spec
