module Hokey.Odds (HoleCards, deck, remainingCards, compareHandToRange) where

import           Hokey.Card
import           Hokey.Hand

type HoleCards = (Card, Card)

type HandResults = (Integer, Integer, Integer)

compareHandToRange :: [Card] -> [Card] -> [[Card]] -> HandResults
compareHandToRange hand board range = foldl (compareTwoHands hand board) (0, 0, 0) range

compareTwoHands :: [Card] -> [Card] -> HandResults -> [Card] -> HandResults
compareTwoHands hand1 board (p1, d, p2) hand2 =
  case (getBestHand (hand1 ++ board), getBestHand (hand2 ++ board)) of
    (Just h1, Just h2) ->
      case (h1 `compare` h2) of
        GT -> (p1 + 1, d, p2)
        LT -> (p1, d, p2 + 1)
        EQ -> (p1, d + 1, p2)
    (_, _) -> error "invalid hands given"

-- takes two player ranges and a board and returns the number of times player 1 wins and player 2
-- wins getOdds :: [HoleCards] -> [HoleCards] -> [Card] -> (Integer, Integer) getOdds p1 p2
-- boardCards = (p2Wins, p2Wins)
--  where
--    p1
ranks :: [Rank]
ranks = [Deuce .. Ace]

cons :: Suit -> Rank -> Card
cons s r = Card r s

deck :: [Card]
deck = (map (cons S) ranks) ++ (map (cons H) ranks) ++ (map (cons D) ranks) ++ (map (cons C) ranks)

remainingCards :: [Card] -> [Card]
remainingCards c = filter (`notElem` c) deck
