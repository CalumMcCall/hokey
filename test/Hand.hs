import Data.List
import Test.Hspec
import Distribution.TestSuite
import Hand
import Card

{-
--Test hasPair

testHasPairNormal = TestCase $ assertEqual "Should get list of cards containing a pair" [Card Deuce Spades, Card Deuce Clubs, Card Ten Spades, Card Eight Diamonds, Card Three Clubs] (hasPair $ reverse $ sort [Card Deuce Spades, Card Deuce Clubs, Card Three Spades, Card Ten Diamonds, Card Eight Clubs])

testHasPairWithHighCards = TestCase $ assertEqual "Should get empty list" [] (hasPair $ reverse $ sort [(Card Deuce Spades), (Card Four Clubs), (Card Three Spades)])

--Test hasTwoPair

testTwoPairNormal = TestCase $ assertEqual "Should get list of cards containing two pair" [Card Ace Diamonds, Card Ace Hearts, Card Three Spades, Card Three Clubs, Card King Diamonds] (hasTwoPair $ reverse $ sort [Card Three Spades, Card Three Clubs, Card King Diamonds, Card Ace Hearts, Card Ace Diamonds])

testTwoPairWithOnePair = TestCase $ assertEqual "Should get empty list" [] (hasTwoPair $ reverse $ sort [Card Seven Spades, Card Seven Clubs, Card Queen Diamonds, Card Ace Hearts, Card King Diamonds])

testTwoPairWithHighCards = TestCase $ 

--Test hasTrips

testTripsNormal = TestCase $ assertEqual "Should get list containing trips" [Card Ten Spades, Card Ten Clubs, Card Ten Diamonds, Card Ace Hearts, Card King Diamonds] (hasTrips $ reverse $ sort [Card Ten Spades, Card Ten Clubs, Card Ten Diamonds, Card King Hearts, Card Ace Diamonds])

testTripsWithOnePair = TestCase $ assertEqual "Should get empty list" [] (hasTrips $ reverse $ sort [Card Ten Spades, Card Ten Clubs, Card Queen Diamonds, Card Ace Hearts, Card King Diamonds])

testTripsWithHighCards = TestCase $ assertEqual "Should get empty list" [] (hasTrips $ reverse $ sort [Card Ten Spades, Card Seven Clubs, Card Queen Diamonds, Card Ace Hearts, Card King Diamonds])

--Test hasStraight

testStraightNothing = TestCase $ assertEqual "Should get empty list" [] (hasStraight $ reverse $ sort [Card Ten Spades, Card Ten Clubs, Card Queen Diamonds, Card Jack Spades, Card Three Clubs])

testStraightAlmost = TestCase $ assertEqual "Should get empty list" [] (hasStraight $ reverse $ sort [Card Six Spades, Card Five Spades, Card Four Spades, Card Three Spades, Card Ace Spades])

testStraightAlmostBroadway = TestCase $ assertEqual "Should get empty list" [] (hasStraight $ reverse $ sort [Card Ace Spades, Card King Diamonds, Card Queen Spades, Card Jack Spades, Card Nine Spades])

testStraightNormal = TestCase $ assertEqual "Should get 6-high straight" [Card Six Spades, Card Five Spades, Card Four Clubs, Card Three Spades, Card Deuce Spades] (hasStraight $ reverse $ sort [Card Six Spades, Card Five Spades, Card Four Clubs, Card Three Spades, Card Deuce Spades])

testStraightWheel = TestCase $ assertEqual "Should get 5-high straight" [Card Five Clubs, Card Four Hearts, Card Three Spades, Card Deuce Spades, Card Ace Spades] (hasStraight $ reverse $ sort [Card Ace Spades, Card Five Clubs, Card Four Hearts, Card Three Spades, Card Deuce Spades])

testStraightBroadway = TestCase $ assertEqual "Should get Broadway Straight" [Card Ace Spades, Card King Diamonds, Card Queen Spades, Card Jack Clubs, Card Ten Spades] (hasStraight $ reverse $ sort [Card Ace Spades, Card King Diamonds, Card Queen Spades, Card Jack Clubs, Card Ten Spades])

--Test hasFlush

testFlushNothing = TestCase $ assertEqual "Should get empty list" [] (hasFlush $ reverse $ sort [Card Nine Spades, Card Queen Clubs, Card Three Diamonds, Card Jack Spades, Card Ten Spades])

testFlushAlmost = TestCase $ assertEqual "Should get empty list" [] (hasFlush $ reverse $ sort [Card Nine Spades, Card Queen Clubs, Card Three Spades, Card Jack Spades, Card Ten Spades])

testFlushNormal = TestCase $ assertEqual "Should get flush" [Card Queen Spades, Card Jack Spades, Card Ten Spades, Card Nine Spades, Card Three Spades] (hasFlush $ reverse $ sort [Card Nine Spades, Card Queen Spades, Card Three Spades, Card Jack Spades, Card Ten Spades])

--Test hasFullHouse

testFullHouseWithHighCards = TestCase $ assertEqual "Should get empty list" [] (hasQuads $ reverse $ sort [Card Ten Spades, Card Seven Clubs, Card Queen Diamonds, Card Ace Hearts, Card King Diamonds])

testFullHouseWithOnePair = TestCase $ assertEqual "Should get empty list" [] (hasFullHouse $ reverse $ sort [Card Ace Hearts, Card King Diamonds, Card Queen Diamonds, Card Ten Spades, Card Ten Clubs])

testFullHouseWithTrips = TestCase $ assertEqual "Should get empty list" [] (hasFullHouse $ reverse $ sort [Card Ace Hearts, Card Ace Diamonds, Card Ace Spades, Card Ten Clubs, Card Nine Diamonds])

testFullHouseNormal = TestCase $ assertEqual "Should get list containing full house" [Card Ten Spades, Card Ten Clubs, Card Nine Diamonds, Card Nine Hearts, Card Nine Spades] (hasFullHouse $ reverse $ sort [Card Ten Spades, Card Ten Clubs, Card Nine Diamonds, Card Nine Hearts, Card Nine Spades])


--Test hasQuads

testQuadsNormal = TestCase $ assertEqual "Should get list containing quads" [Card Ten Spades, Card Ten Clubs, Card Ten Diamonds, Card Ten Hearts] (hasQuads $ reverse $ sort [Card Ten Spades, Card Ten Clubs, Card Ten Diamonds, Card Ten Hearts, Card King Diamonds])

testQuadsWithTrips = TestCase $ assertEqual "Should get empty list" [] (hasQuads $ reverse $ sort [Card Ace Hearts, Card King Diamonds, Card Ten Spades, Card Ten Clubs, Card Ten Diamonds])

testQuadsWithOnePair = TestCase $ assertEqual "Should get empty list" [] (hasQuads $ reverse $ sort [Card Ten Spades, Card Ten Clubs, Card Queen Diamonds, Card Ace Hearts, Card King Diamonds])

testQuadsWithHighCards = TestCase $ assertEqual "Should get empty list" [] (hasQuads $ reverse $ sort [Card Ten Spades, Card Seven Clubs, Card Queen Diamonds, Card Ace Hearts, Card King Diamonds])


--Test hasStraightFlush

testSFNormal = TestCase $ assertEqual "Should get 6-high straight flush" [Card Six Spades, Card Five Spades, Card Four Spades, Card Three Spades, Card Deuce Spades] (hasStraightFlush $ reverse $ sort [Card Six Spades, Card Five Spades, Card Four Spades, Card Three Spades, Card Deuce Spades])

testSFSteelWheel = TestCase $ assertEqual "Should get 5-high straight flush" [Card Five Spades, Card Four Spades, Card Three Spades, Card Deuce Spades, Card Ace Spades] (hasStraightFlush $ reverse $ sort [Card Ace Spades, Card Five Spades, Card Four Spades, Card Three Spades, Card Deuce Spades])

testSFRoyalFlush = TestCase $ assertEqual "Should get Royal flush" [Card Ace Spades, Card King Spades, Card Queen Spades, Card Jack Spades, Card Ten Spades] (hasStraightFlush $ reverse $ sort [Card Ace Spades, Card King Spades, Card Queen Spades, Card Jack Spades, Card Ten Spades])

testSFAlmost = TestCase $ assertEqual "Should get empty list" [] (hasStraightFlush $ reverse $ sort [Card Ace Spades, Card Six Spades, Card Five Spades, Card Four Spades, Card Three Spades])

testSFNothing = TestCase $ assertEqual "Should get empty list" [] (hasStraightFlush $ reverse $ sort [Card Ten Spades, Card Ten Clubs, Card Queen Diamonds, Card Jack Spades, Card Three Clubs])


--Test getBestHand

testStraightLTRoyal = TestCase $ assertEqual "Straight is less than a royal flush" True ((getBestHand $ reverse $ sort [Card Six Clubs, Card Five Spades, Card Four Diamonds, Card Three Clubs, Card Deuce Spades]) < (getBestHand $ reverse $ sort [Card Ace Spades, Card King Spades, Card Queen Spades, Card Jack Spades, Card Ten Spades]))

testHighCardsLTTwoPair = TestCase $ assertEqual "High cards are less than two pair" True ((getBestHand $ reverse $ sort [Card Six Clubs, Card Ten Spades, Card Deuce Diamonds, Card King Clubs, Card Deuce Spades]) < (getBestHand $ reverse $ sort [Card Ace Spades, Card Ace Clubs, Card Eight Diamonds, Card Deuce Clubs, Card Eight Spades]))

testStraightGTWheel = TestCase $ assertEqual "6-high straight is greater than a wheel" True ((getBestHand $ reverse $ sort [Card Six Clubs, Card Five Spades, Card Four Diamonds, Card Three Clubs, Card Deuce Spades]) > (getBestHand $ reverse $ sort [Card Deuce Spades, Card Four Clubs, Card Five Diamonds, Card Three Hearts, Card Ace Spades]))

testCases :: [(String, Test)]
            ("Test hasTwoPair with high cards", testTwoPairWithHighCards),
            ("Test hasTwoPair with one pair", testTwoPairWithOnePair),
            ("Test hasTwoPair with normal input", testTwoPairNormal),
            ("Test hasTrips with high cards", testTripsWithHighCards),
            ("Test hasTrips with one pair", testTripsWithOnePair),
            ("Test hasTrips with normal input", testTripsNormal),
            ("Test hasStraight with no hand", testStraightNothing),
            ("Test hasStraight with 4-card straight", testStraightAlmost),
            ("Test hasStraight with 4-card broadway straight", testStraightAlmostBroadway),
            ("Test hasStraight with normal straight", testStraightNormal),
            ("Test hasStraight with wheel", testStraightWheel),
            ("Test hasStraight with Broadway straight", testStraightBroadway),
            ("Test hasFlush with no hand", testFlushNothing),
            ("Test hasFlush with 4-card flush", testFlushAlmost),
            ("Test hasFlush with normal flush", testFlushNormal),
            ("Test hasFullHouse with high cards", testFullHouseWithHighCards),
            ("Test hasFullHouse with one pair", testFullHouseWithOnePair),
            ("Test hasFullHouse with trips", testFullHouseWithTrips),
            ("Test hasFullHouse with normal full house", testFullHouseWithHighCards),
            ("Test hasQuads with high cards", testQuadsWithHighCards),
            ("Test hasQuads with one pair", testQuadsWithOnePair),
            ("Test hasQuads with trips", testQuadsWithTrips),
            ("Test hasQuads with normal hand", testQuadsNormal),
            ("Test hasStraightFlush with no hand", testSFNothing),
            ("Test hasStraightFlush with 4-card straight", testSFAlmost),
            ("Test hasStraightFlush with normal input", testSFNormal),
            ("Test hasStraightFlush with 5-high straight flush", testSFSteelWheel),
            ("Test hasStraightFlush with royal flush", testSFRoyalFlush),
            ("Test Hand Ord function with royal flush and straight", testStraightLTRoyal),
            ("Test Hand Ord function with high cards and straight", testHighCardsLTTwoPair),
            ("Test Hand Ord function with two straights", testStraightGTWheel)
            ]
-}

main :: IO ()
main = hspec $ do
  describe "hasPair" $ do
    it "with high cards should get empty list" $ do
      (hasPair $ reverse $ sort [(Card Deuce Spades), (Card Four Clubs), (Card Three Spades)]) `shouldBe` []
    it "hasPair with normal input" $ do
       (hasPair $ reverse $ sort [Card Deuce Spades, Card Deuce Clubs, Card Three Spades, Card Ten Diamonds, Card Eight Clubs]) `shouldBe` [Card Deuce Spades, Card Deuce Clubs, Card Ten Spades, Card Eight Diamonds, Card Three Clubs]
    it "hasTwoPair with high cards" $ do
       (hasTwoPair $ reverse $ sort [Card Ten Spades, Card Seven Clubs, Card Queen Diamonds, Card Ace Hearts, Card King Diamonds]) `shouldBe` []