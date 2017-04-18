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
      blackQueens = [Card Queen S, Card Queen C]
      redJacks = [Card Jack S, Card Jack C]
      kkqqjj = [blackKings, blackQueens, redJacks]
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
      compareHandToRange kkqqjj blankBoard redAces `shouldBe` (1, 0, 2)

  describe "removeDeadCards" $ do
    it "removes correct cards" $ do
      removeDeadCards blankBoard aaqq `shouldBe` [[As,Ac],[As,Ah],[As,Ad],[Ac,Ah],[Ac,Ad],[Ah,Ad],[Qc,Qh],[Qc,Qd],[Qh,Qd]]

  describe "compareRangeToRange" $ do
    it "returns draws correctly" $ do
      compareRangeToRange kkqqjj blankBoard kkqqjj `shouldBe` (3,0,3)
    it "return a win vs a single hand correctly" $ do
      putStrLn $ show $ map (compareHandToRange (getPairs Queen) blankBoard) aaqq
      compareRangeToRange aaqq blankBoard (getPairs Queen) `shouldBe` (0,6,36)
    it "return a 2:3 win ratio correctly" $ do
      let results = compareRangeToRange aaqq blankBoard aakk
      printEquity results
      results `shouldBe` (36,6,72)
