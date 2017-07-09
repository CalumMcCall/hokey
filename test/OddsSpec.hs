module OddsSpec (main, spec) where

import           Hokey.Card
import           Hokey.Odds
import           Test.Hspec

main :: IO ()
main = hspec spec

getPairs :: Rank -> [[Card]]
getPairs r  = cards
  where
    suits = [[S,C],[S,H],[S,D],[C,H],[C,D],[H,D]]
    cards = map (\x -> map (Card r) x) suits

spec :: Spec
spec = do
  let redAces = [Card Ace H, Card Ace D]
      blackAces = [Card Ace S, Card Ace C]
      blankBoard = [Card King H, Card Five C, Card Queen S, Card Deuce C, Card Nine C]
      blackKings = [Card King S, Card King C]
      redJacks = [Card Jack S, Card Jack C]
      kkqqjj = getPairs King ++ getPairs Queen ++ getPairs Jack
      aaqq = getPairs Ace ++ getPairs Queen
      aakk = getPairs Ace ++ getPairs King
  describe "remainingCards" $ do
    it "removes correct card" $ do
      remainingCards (Card Deuce S : []) `shouldSatisfy` ranksEqual (tail deck)
    it "removes correct cards" $ do
      remainingCards (Card Deuce S : Card Ace C : []) `shouldSatisfy` ranksEqual
                                                                        (tail $ filter
                                                                                  (\x ->
                                                                                     x /= Card Ace C)
                                                                                  deck)

  describe "compareHandToRange" $ do
    it "recognises draws" $ do
      compareHandToRange [blackAces] blankBoard redAces `shouldBe` (0, 1, 0)
    it "recognises winning hands" $ do
      compareHandToRange [redJacks] blankBoard redAces `shouldBe` (1, 0, 0)
    it "recognises losing hands" $ do
      compareHandToRange [blackKings] blankBoard redAces `shouldBe` (0, 0, 1)
    it "recognises losing ranges" $ do
      compareHandToRange kkqqjj blankBoard redAces `shouldBe` (6, 0, 12)

  describe "removeDeadCards" $ do
    it "removes correct cards" $ do
      length (removeDeadCards blankBoard aaqq) `shouldBe` 9

  describe "compareRangeToRange" $ do
    it "returns draws correctly" $ do
      compareRangeToRange kkqqjj blankBoard kkqqjj `shouldBe` (45,6,45)
    it "return a win vs a single hand correctly" $ do
      compareRangeToRange aaqq blankBoard (getPairs Queen) `shouldBe` (0,0,18)
    it "return a 2:3 win ratio correctly" $ do
      let results = compareRangeToRange aaqq blankBoard aakk
      let equity = getEquity2 results
      results `shouldBe` (18,6,27)
      equity `shouldBe` ((35.29, 41.18, 5.88),(52.94,58.82,5.88))

