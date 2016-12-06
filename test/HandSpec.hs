module HandSpec (main, spec) where

import Test.Hspec
import Hokey.Hand
import Hokey.Card

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let twoPairPairedBoard = [Card Ace D,
                            Card Ace H,
                            Card Three S,
                            Card Three C,
                            Card King D]
      pair = [Card Deuce S,
              Card Deuce C,
              Card Ten S,
              Card Eight D,
              Card Three C]
      trips = [Card Ten S,
               Card Ten C,
               Card Ten D,
               Card Ace H,
               Card King D]
      sixHighStraight = [Card Six S,
                         Card Five S,
                         Card Four C,
                         Card Three S,
                         Card Deuce S]
      fiveHighStraight = [Card Five C,
                          Card Four H,
                          Card Three S,
                          Card Deuce S,
                          Card Ace S]
      broadwayStraight = [Card Ace S,
                          Card King D,
                          Card Queen S,
                          Card Jack C,
                          Card Ten S]
      flush = [Card Queen S,
               Card Jack S,
               Card Ten S,
               Card Nine S,
               Card Three S]
      fullHouse = [Card Nine S,
                   Card Nine D,
                   Card Nine H,
                   Card Ten C,
                   Card Ten S]
      quadsAHigh = [Card Ten S,
                    Card Ten C,
                    Card Ten D,
                    Card Ten H,
                    Card Ace S]
      quadsKHigh = [Card Ten S,
                    Card Ten C,
                    Card Ten D,
                    Card Ten H,
                    Card King S]
      fourCardSF = [Card Ace S,
                    Card Six S,
                    Card Five S,
                    Card Four S,
                    Card Three S]
      sixHighSF = [Card Six S,
                   Card Five S,
                   Card Four S,
                   Card Three S,
                   Card Deuce S]
      steelWheel = [Card Five S,
                    Card Four S,
                    Card Three S,
                    Card Deuce S,
                    Card Ace S]
      royalFlush = [Card Ace S,
                    Card King S,
                    Card Queen S,
                    Card Jack S,
                    Card Ten S]

  describe "hasPair" $ do
    it "returns nothing when passed high cards" $ do
      hasPair [Card Deuce S, Card Four C, Card Three S] `shouldBe` []
    it "returns pair correctly" $ do
      hasPair pair `shouldBe` pair
  describe "hasTwoPair" $ do
    it "returns nothing when passed high cards" $ do
      hasTwoPair [Card Ten S,
                   Card Seven C,
                   Card Queen D,
                   Card Ace H,
                   Card King D] `shouldBe` []
    it "returns two pair correctly" $ do
      hasTwoPair twoPairPairedBoard `shouldBe` twoPairPairedBoard
    it "returns two pair on paired board" $ do
      hasTwoPair [Card Seven S,
                   Card Seven C,
                   Card Queen D,
                   Card Ace H,
                   Card King D] `shouldBe` []
  describe "hasTrips" $ do
    it "returns high cards correctly" $ do
      hasTrips [Card Ten S, Card Seven C, Card Queen D, Card Ace H, Card King D] `shouldBe` []
    it "returns pairs correctly" $ do
      hasTrips [Card Ten S, Card Ten C, Card Queen D, Card Ace H, Card King D] `shouldBe` []
    it "returns trips correctly" $ do
      hasTrips trips `shouldBe` trips
  describe "hasStraight" $ do
    it "rejects pairs correctly" $ do
      hasStraight pair `shouldBe` []
    it "rejects 4-card straights" $ do
      hasStraight [Card Six S, Card Five S, Card Four S, Card Three S, Card Ace S] `shouldBe` []
    it "rejects 4-card broadway straights" $ do
      hasStraight [Card Ace S, Card King D, Card Queen S, Card Jack S, Card Nine S] `shouldBe` []
    it "returns value 6-high straight" $ do
      hasStraight sixHighStraight `shouldBe` sixHighStraight
    it "returns 5-high straight" $ do
      hasStraight fiveHighStraight `shouldBe` fiveHighStraight
    it "returns a broadway straight" $ do
      hasStraight broadwayStraight `shouldBe` broadwayStraight

  describe "hasFlush" $ do
    it "returns high cards correctly" $ do
      hasFlush [Card Nine S, Card Queen C, Card Three D, Card Jack S, Card Ten S] `shouldBe` []
    it "handles 4-card flush" $ do
      hasFlush [Card Nine S, Card Queen C, Card Three S, Card Jack S, Card Ten S] `shouldBe` []
    it "handles normal flush" $ do
     hasFlush flush `shouldBe` flush

  describe "hasFullHouse" $ do
    it "handles pairs correctly" $ do
      hasFullHouse [Card Ace H, Card King D, Card Queen D, Card Ten S, Card Ten C] `shouldBe` []
    it "handles trips correctly" $ do
      hasFullHouse trips `shouldBe` []
    it "recognises full house correctly" $ do
      hasFullHouse fullHouse `shouldBe` fullHouse

  describe "hasQuads" $ do
    it "handles high cards correctly" $ do
      hasQuads [Card Ten S, Card Seven C, Card Queen D, Card Ace H, Card King D] `shouldBe` []
    it "handles pairs correctly" $ do
      hasQuads pair `shouldBe` []
    it "handles trips correctly" $ do
      hasQuads trips `shouldBe` []
    it "recognises quadsAHigh" $ do
      hasQuads quadsAHigh `shouldBe` quadsAHigh

  describe "hasStraightFlush" $ do
    it "handles pairs correctly" $ do
      hasStraightFlush [Card Ten S, Card Ten C, Card Queen D, Card Jack S, Card Three C] `shouldBe` []
    it "handles 4-card straights" $ do
      hasStraightFlush fourCardSF `shouldBe` []
    it "handles 6-high straight flush" $ do
      hasStraightFlush sixHighSF `shouldBe` sixHighSF
    it "recognises steel wheel" $ do
      hasStraightFlush steelWheel `shouldBe` steelWheel
    it "recognises royal flushes" $ do
      hasStraightFlush royalFlush `shouldBe` royalFlush

  describe "getBestHand" $ do
    it "straight less than royal flush" $ do
      getBestHand [Card Six C, Card Five S, Card Four D, Card Three C, Card Deuce S] < getBestHand [Card Ace S, Card King S, Card Queen S, Card Jack S, Card Ten S] `shouldBe` True
    it "high cards less than two pair" $ do
      getBestHand [Card Six C, Card Ten S, Card Deuce D, Card King C, Card Deuce S] < getBestHand [Card Ace S, Card Ace C, Card Eight D, Card Deuce C, Card Eight S] `shouldBe` True
    it "6-high straight greater than wheel" $ do
      getBestHand [Card Six C, Card Five S, Card Four D, Card Three C, Card Deuce S] > getBestHand [Card Deuce S, Card Four C, Card Five D, Card Three H, Card Ace S] `shouldBe` True
    it "high card counts with quads" $ do
      getBestHand quadsAHigh > getBestHand quadsKHigh `shouldBe` True
