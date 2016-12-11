module OddsSpec (main, spec) where

import           Hokey.Card
import           Hokey.Odds
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "remainingCards" $ do
    it "removes correct card" $ do
      remainingCards (Card Deuce S:[]) `shouldSatisfy` ranksEqual (tail deck)
    it "removes correct cards" $ do
      remainingCards (Card Deuce S:Card Ace C:[]) `shouldSatisfy` ranksEqual (tail $ filter (\x -> x /= Card Ace C) deck)
