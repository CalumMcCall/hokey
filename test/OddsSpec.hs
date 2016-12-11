module OddsSpec (main, spec) where

import           Hokey.Card
import           Hokey.Odds
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let redAces = [Card Ace H, Card Ace D]
      blackAces = [Card Ace S, Card Ace C]
      blankBoard = [Card King H, Card Five C, Card Queen S, Card Deuce C, Card Nine C]
      blackKings = [Card King S, Card King C]
      blackQueens = [Card Queen S, Card Queen C]
      redJacks = [Card Jack S, Card Jack C]
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
      compareHandToRange redAces blankBoard [blackAces] `shouldBe` (0, 1, 0)
    it "recognises winning hands" $ do
      compareHandToRange redAces blankBoard [redJacks] `shouldBe` (1, 0, 0)
    it "recognises losing hands" $ do
      compareHandToRange redAces blankBoard [blackKings] `shouldBe` (0, 0, 1)
    it "recognises losing ranges" $ do
      compareHandToRange redAces blankBoard [blackKings, blackQueens, redJacks] `shouldBe` (1, 0, 2)
