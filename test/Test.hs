module Test where
import Test.HUnit
import qualified Distribution.TestSuite as TS
import qualified Distribution.TestSuite.HUnit as H
import Card

--Test hasPair

testHasPairNormal = TestCase $ assertEqual "Should get a list of cards containing a pair" [(Card Two Spades), (Card Two Clubs)] (hasPair [(Card Two Spades), (Card Two Clubs), (Card Three Spades)])

testHasPairWithHighCards = TestCase $ assertEqual "Should get empty list" [] (hasPair [(Card Two Spades), (Card Four Clubs), (Card Three Spades)])

--Test hasTwoPair

testTwoPairNormal = TestCase $ assertEqual "Should get a list of cards containing two pair" [Card Three Spades, Card Three Clubs, Card Ace Diamonds, Card Ace Hearts] (hasTwoPair [Card Three Spades, Card Three Clubs, Card Ace Diamonds, Card Ace Hearts, Card King Diamonds])

testTwoPairWithOnePair = TestCase $ assertEqual "Should get empty list" [] (hasTwoPair [Card Seven Spades, Card Seven Clubs, Card Queen Diamonds, Card Ace Hearts, Card King Diamonds])

testTwoPairWithHighCards = TestCase $ assertEqual "Should get empty list" [] (hasTwoPair [Card Ten Spades, Card Seven Clubs, Card Queen Diamonds, Card Ace Hearts, Card King Diamonds])

--Test hasTrips

testTripsWithHighCards = TestCase $ assertEqual "Should get empty list" [] (hasTrips [Card Ten Spades, Card Seven Clubs, Card Queen Diamonds, Card Ace Hearts, Card King Diamonds])

testTripsWithOnePair = TestCase $ assertEqual "Should get empty list" [] (hasTrips [Card Ten Spades, Card Ten Clubs, Card Queen Diamonds, Card Ace Hearts, Card King Diamonds])

testTripsNormal = TestCase $ assertEqual "Should get list containing trips" [Card Ten Spades, Card Ten Clubs, Card Ten Diamonds] (hasTrips [Card Ten Spades, Card Ten Clubs, Card Ten Diamonds, Card Ace Hearts, Card King Diamonds])

--Test hasStraight

testStraightNothing = TestCase $ assertEqual "Should get empty list" [] (hasStraight [Card Ten Spades, Card Ten Clubs, Card Queen Diamonds, Card Jack Spades, Card Three Clubs])

testStraightAlmost = TestCase $ assertEqual "Should get empty list" [] (hasStraight [Card Six Spades, Card Five Spades, Card Four Spades, Card Three Spades, Card Ace Spades])

testStraightAlmostBroadway = TestCase $ assertEqual "Should get Broadway Straight" [] (hasStraight [Card Ace Spades, Card King Diamonds, Card Queen Spades, Card Jack Spades, Card Nine Spades])

testStraightNormal = TestCase $ assertEqual "Should get 6-high straight flush" [Card Six Spades, Card Five Spades, Card Four Spades, Card Three Spades, Card Two Spades] (hasStraight [Card Six Spades, Card Five Spades, Card Four Spades, Card Three Spades, Card Two Spades])

testStraightWheel = TestCase $ assertEqual "Should get 5-high straight" [Card Five Clubs, Card Four Hearts, Card Three Spades, Card Two Spades, Card Ace Spades] (hasStraight [Card Ace Spades, Card Five Clubs, Card Four Hearts, Card Three Spades, Card Two Spades])

testStraightBroadway = TestCase $ assertEqual "Should get Broadway Straight" [Card Ace Spades, Card King Diamonds, Card Queen Spades, Card Jack Spades, Card Ten Spades] (hasStraight [Card Ace Spades, Card King Diamonds, Card Queen Spades, Card Jack Spades, Card Ten Spades])

--Test hasStraightFlush

testSFNoHand = TestCase $ assertEqual "Should get empty list" [] (hasStraightFlush [Card Ten Spades, Card Ten Clubs, Card Queen Diamonds, Card Jack Spades, Card Three Clubs])

testSFAlmost = TestCase $ assertEqual "Should get empty list" [] (hasStraightFlush [Card Six Spades, Card Five Spades, Card Four Spades, Card Three Spades, Card Ace Spades])

testSFNormal = TestCase $ assertEqual "Should get 6-high straight flush" [Card Six Spades, Card Five Spades, Card Four Spades, Card Three Spades, Card Two Spades] (hasStraightFlush [Card Six Spades, Card Five Spades, Card Four Spades, Card Three Spades, Card Two Spades])

testSFSteelWheel = TestCase $ assertEqual "Should get 5-high straight flush" [Card Five Spades, Card Four Spades, Card Three Spades, Card Two Spades, Card Ace Spades] (hasStraightFlush [Card Ace Spades, Card Five Spades, Card Four Spades, Card Three Spades, Card Two Spades])

testSFRoyalFlush = TestCase $ assertEqual "Should get Royal flush" [Card Ace Spades, Card King Spades, Card Queen Spades, Card Jack Spades, Card Ten Spades] (hasStraightFlush [Card Ace Spades, Card King Spades, Card Queen Spades, Card Jack Spades, Card Ten Spades])

testCases :: [(String, Test)]
testCases = [("Test hasPair with high cards", testHasPairWithHighCards),
            ("Test hasPair with normal input", testHasPairNormal),
            ("Test hasTwoPair with high cards", testTwoPairWithHighCards),
            ("Test hasTwoPair with one pair", testTwoPairWithOnePair),
            ("Test hasTwoPair with normal input", testTwoPairNormal),
            ("Test hasTrips with high cards", testTripsWithHighCards),
            ("Test hasTrips with one pair", testTripsWithOnePair),
            ("Test hasTrips with normal input", testTripsNormal),
            ("Test hasStraight with no hand", testStraightNothing),
            ("Test hasStraight with normal straight", testStraightNormal),
            ("Test hasStraight with 4-card straight", testStraightAlmost),
            ("Test hasStraight with 4-card broadway straight", testStraightAlmostBroadway),
            ("Test hasStraight with wheel", testStraightWheel),
            ("Test hasStraight with Broadway straight", testStraightBroadway),
            ("Test hasStraightFlush with empty list", testSFNoHand),
            ("Test hasStraightFlush with 4-card straight", testSFAlmost),
            ("Test hasStraightFlush with normal input", testSFNormal),
            ("Test hasStraightFlush with 5-high straight flush", testSFSteelWheel),
            ("Test hasStraightFlush with royal flush", testSFRoyalFlush)
            ]

tests :: IO [TS.Test]
tests = return $ map (uncurry H.test) testCases

