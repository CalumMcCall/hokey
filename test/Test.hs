module Test where
import Test.HUnit
import qualified Distribution.TestSuite as TS
import qualified Distribution.TestSuite.HUnit as H
import Card

testHasPair = TestCase $ assertEqual "Should get a list of cards with a pair here" (Just [(Card Two Spades), (Card Two Clubs)]) (hasPair [(Card Two Spades), (Card Two Clubs), (Card Three Spades)])

testCases :: [(String, Test)]
testCases = [("Test hasPair", testHasPair)]

tests :: IO [TS.Test]
tests = return $ map (uncurry H.test) testCases

