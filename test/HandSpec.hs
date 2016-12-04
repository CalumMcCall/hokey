module HandSpec (main, spec) where

import Data.List
import Test.Hspec
import Hokey.Hand
import Hokey.Card

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let twoPairPairedBoard = [Card Ace Diamonds,
                            Card Ace Hearts,
                            Card Three Spades,
                            Card Three Clubs,
                            Card King Diamonds]
      pair = [Card Deuce Spades,
              Card Deuce Clubs,
              Card Ten Spades,
              Card Eight Diamonds,
              Card Three Clubs]
      trips = [Card Ten Spades,
               Card Ten Clubs,
               Card Ten Diamonds,
               Card Ace Hearts,
               Card King Diamonds]
      sixHighStraight = [Card Six Spades,
                         Card Five Spades,
                         Card Four Clubs,
                         Card Three Spades,
                         Card Deuce Spades]
      fiveHighStraight = [Card Five Clubs,
                          Card Four Hearts,
                          Card Three Spades,
                          Card Deuce Spades,
                          Card Ace Spades]
      broadwayStraight = [Card Ace Spades,
                          Card King Diamonds,
                          Card Queen Spades,
                          Card Jack Clubs,
                          Card Ten Spades]
      flush = [Card Queen Spades,
               Card Jack Spades,
               Card Ten Spades,
               Card Nine Spades,
               Card Three Spades]
      fullHouse = [Card Nine Spades,
                   Card Nine Diamonds,
                   Card Nine Hearts,
                   Card Ten Clubs,
                   Card Ten Spades]
      quads = [Card Ten Spades,
               Card Ten Clubs,
               Card Ten Diamonds,
               Card Ten Hearts,
               Card Ace Spades]
      fourCardSF = [Card Ace Spades,
                    Card Six Spades,
                    Card Five Spades,
                    Card Four Spades,
                    Card Three Spades]
      sixHighSF = [Card Six Spades,
                   Card Five Spades,
                   Card Four Spades,
                   Card Three Spades,
                   Card Deuce Spades]
      steelWheel = [Card Five Spades,
                    Card Four Spades,
                    Card Three Spades,
                    Card Deuce Spades,
                    Card Ace Spades]
      royalFlush = [Card Ace Spades,
                    Card King Spades,
                    Card Queen Spades,
                    Card Jack Spades,
                    Card Ten Spades]

  describe "hasPair" $ do
    it "returns nothing when passed high cards" $ do
      (hasPair $ reverse $ sort [(Card Deuce Spades), (Card Four Clubs), (Card Three Spades)]) `shouldBe` []
    it "returns pair correctly" $ do
      (hasPair pair) `shouldBe` pair
  describe "hasTwoPair" $ do
    it "returns nothing when passed high cards" $ do
      (hasTwoPair [Card Ten Spades,
                   Card Seven Clubs,
                   Card Queen Diamonds,
                   Card Ace Hearts,
                   Card King Diamonds]) `shouldBe` []
    it "returns two pair correctly" $ do
      (hasTwoPair twoPairPairedBoard) `shouldBe` twoPairPairedBoard
    it "returns two pair on paired board" $ do
      (hasTwoPair [Card Seven Spades,
                   Card Seven Clubs,
                   Card Queen Diamonds,
                   Card Ace Hearts,
                   Card King Diamonds]) `shouldBe` []
  describe "hasTrips" $ do
    it "returns high cards correctly" $ do
      (hasTrips [Card Ten Spades, Card Seven Clubs, Card Queen Diamonds, Card Ace Hearts, Card King Diamonds]) `shouldBe` []
    it "returns pairs correctly" $ do
      (hasTrips [Card Ten Spades, Card Ten Clubs, Card Queen Diamonds, Card Ace Hearts, Card King Diamonds]) `shouldBe` []
    it "returns trips correctly" $ do
      (hasTrips trips) `shouldBe` trips
  describe "hasStraight" $ do
    it "rejects pairs correctly" $ do
      (hasStraight pair) `shouldBe` []
    it "rejects 4-card straights" $ do
      (hasStraight [Card Six Spades, Card Five Spades, Card Four Spades, Card Three Spades, Card Ace Spades]) `shouldBe` []
    it "rejects 4-card broadway straights" $ do
      (hasStraight [Card Ace Spades, Card King Diamonds, Card Queen Spades, Card Jack Spades, Card Nine Spades]) `shouldBe` []
    it "returns value 6-high straight" $ do
      (hasStraight sixHighStraight) `shouldBe` sixHighStraight
    it "returns 5-high straight" $ do
      (hasStraight fiveHighStraight) `shouldBe` fiveHighStraight
    it "returns a broadway straight" $ do
      (hasStraight broadwayStraight) `shouldBe` broadwayStraight

  describe "hasFlush" $ do
    it "returns high cards correctly" $ do
      (hasFlush [Card Nine Spades, Card Queen Clubs, Card Three Diamonds, Card Jack Spades, Card Ten Spades]) `shouldBe` []
    it "handles 4-card flush" $ do
      (hasFlush [Card Nine Spades, Card Queen Clubs, Card Three Spades, Card Jack Spades, Card Ten Spades]) `shouldBe` []
    it "handles normal flush" $ do
     (hasFlush flush) `shouldBe` flush

  describe "hasFullHouse" $ do
    it "handles pairs correctly" $ do
      (hasFullHouse [Card Ace Hearts, Card King Diamonds, Card Queen Diamonds, Card Ten Spades, Card Ten Clubs]) `shouldBe` []
    it "handles trips correctly" $ do
      (hasFullHouse trips) `shouldBe` []
    it "recognises full house correctly" $ do
     (hasFullHouse fullHouse) `shouldBe` fullHouse

  describe "hasQuads" $ do
    it "handles high cards correctly" $ do
      (hasQuads [Card Ten Spades, Card Seven Clubs, Card Queen Diamonds, Card Ace Hearts, Card King Diamonds]) `shouldBe` []
    it "handles pairs correctly" $ do
      (hasQuads pair) `shouldBe` []
    it "handles trips correctly" $ do
      (hasQuads trips) `shouldBe` []
    it "recognises quads" $ do
       (hasQuads quads) `shouldBe` quads

  describe "hasStraightFlush" $ do
    it "handles pairs correctly" $ do
      (hasStraightFlush $ reverse $ sort [Card Ten Spades, Card Ten Clubs, Card Queen Diamonds, Card Jack Spades, Card Three Clubs]) `shouldBe` []
    it "handles 4-card straights" $ do
      (hasStraightFlush $ reverse $ sort fourCardSF) `shouldBe` []
    it "handles 6-high straight flush" $ do
       (hasStraightFlush $ reverse $ sort sixHighSF) `shouldBe` sixHighSF
    it "recognises steel wheel" $ do
      (hasStraightFlush $ reverse $ sort steelWheel) `shouldBe` steelWheel
    it "recognises royal flushes" $ do
      (hasStraightFlush $ reverse $ sort royalFlush) `shouldBe` royalFlush

  describe "getBestHand" $ do
    it "straight less than royal flush" $ do
      ((getBestHand $ reverse $ sort [Card Six Clubs, Card Five Spades, Card Four Diamonds, Card Three Clubs, Card Deuce Spades]) < (getBestHand $ reverse $ sort [Card Ace Spades, Card King Spades, Card Queen Spades, Card Jack Spades, Card Ten Spades])) `shouldBe` True
    it "high cards less than two pair" $ do
      ((getBestHand $ reverse $ sort [Card Six Clubs, Card Ten Spades, Card Deuce Diamonds, Card King Clubs, Card Deuce Spades]) < (getBestHand $ reverse $ sort [Card Ace Spades, Card Ace Clubs, Card Eight Diamonds, Card Deuce Clubs, Card Eight Spades])) `shouldBe` True
    it "6-high straight greater than wheel" $ do
      ((getBestHand $ reverse $ sort [Card Six Clubs, Card Five Spades, Card Four Diamonds, Card Three Clubs, Card Deuce Spades]) > (getBestHand $ reverse $ sort [Card Deuce Spades, Card Four Clubs, Card Five Diamonds, Card Three Hearts, Card Ace Spades])) `shouldBe` True



