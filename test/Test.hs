module Test where
import Test.HUnit
import qualified Distribution.TestSuite as TS
import qualified Distribution.TestSuite.HUnit as H
import Card

testHasPairNormal = TestCase $ assertEqual "Should get a list of cards containing a pair" (Just [(Card Two Spades), (Card Two Clubs)]) (hasPair [(Card Two Spades), (Card Two Clubs), (Card Three Spades)])

testHasPairWithHighCards = TestCase $ assertEqual "Should get Nothing here" Nothing (hasPair [(Card Two Spades), (Card Four Clubs), (Card Three Spades)])

testTwoPairNormal = TestCase $ assertEqual "Should get a list of cards containing two pair" (Just [Card Three Spades, Card Three Clubs, Card Ace Diamonds, Card Ace Hearts]) (hasTwoPair [Card Three Spades, Card Three Clubs, Card Ace Diamonds, Card Ace Hearts, Card King Diamonds])

testTwoPairWithOnePair = TestCase $ assertEqual "Should get Nothing" Nothing (hasTwoPair [Card Seven Spades, Card Seven Clubs, Card Queen Diamonds, Card Ace Hearts, Card King Diamonds])

testTwoPairWithHighCards = TestCase $ assertEqual "Should get Nothing" Nothing (hasTwoPair [Card Ten Spades, Card Seven Clubs, Card Queen Diamonds, Card Ace Hearts, Card King Diamonds])

testTripsWithHighCards = TestCase $ assertEqual "Should get Nothing" Nothing (hasTwoPair [Card Ten Spades, Card Seven Clubs, Card Queen Diamonds, Card Ace Hearts, Card King Diamonds])

testTripsWithOnePair = TestCase $ assertEqual "Should get Nothing" Nothing (hasTwoPair [Card Ten Spades, Card Ten Clubs, Card Queen Diamonds, Card Ace Hearts, Card King Diamonds])

testTripsNothing = TestCase $ assertEqual "Should get Nothing" Nothing (hasTwoPair [Card Ten Spades, Card Ten Clubs, Card Ten Diamonds, Card Ace Hearts, Card King Diamonds])

testSFNothing = TestCase $ assertEqual "Should get Nothing" [] (hasStraightFlush [Card Ten Spades, Card Ten Clubs, Card Queen Diamonds, Card Jack Spades, Card Three Clubs])

testSFNormal = TestCase $ assertEqual "Should get 6-high straight flush" [Card Six Spades, Card Five Spades, Card Four Spades, Card Three Spades, Card Two Spades] (hasStraightFlush [Card Six Spades, Card Five Spades, Card Four Spades, Card Three Spades, Card Two Spades])

testCases :: [(String, Test)]
testCases = [("Test hasPair with high cards", testHasPairWithHighCards),
            ("Test hasPair with normal input", testHasPairNormal),
            ("Test hasTwoPair with high cards", testTwoPairWithHighCards),
            ("Test hasTwoPair with one pair", testTwoPairWithOnePair),
            ("Test hasTwoPair with normal input", testTwoPairNormal),
            ("Test hasTrips with high cards", testTripsWithHighCards),
            ("Test hasTrips with one pair", testTripsWithOnePair),
            ("Test hasTrips with normal input", testTripsNothing),
            ("Test hasStraightFlush with empty list", testSFNothing),
            ("Test hasStraightFlush with normal input", testSFNormal)
            ]

tests :: IO [TS.Test]
tests = return $ map (uncurry H.test) testCases

