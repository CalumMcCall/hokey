module Hokey.Odds (HoleCards, deck, remainingCards, compareHandToRange, compareRangeToRange, removeDeadCards, getEquity2) where

import           Hokey.Card
import           Hokey.Hand

type HoleCards = (Card, Card)

--(Player 1 wins, Draw, Player 2 wins)
type HandResults = (Integer, Integer, Integer)
--(win %, total equity, draw equity)
type PlayerEquity = (Double, Double, Double)
type Equity2 = (PlayerEquity, PlayerEquity)


getEquity2 :: HandResults -> Equity2
getEquity2 (p1,d,p2) = ((roundTo2 p1Win, roundTo2 p1Equity, roundTo2 tieEquity), (roundTo2 p2Win, roundTo2 p2Equity, roundTo2 tieEquity))
  where
    totalHands = p1 + d + p2
    roundTo2 f = (fromIntegral (round $ f * (10^2))) / (10.0^^2)
    getPer x = fromIntegral x / fromIntegral totalHands * (100 :: Double)
    p1Win = getPer p1
    p1Equity = getPer p1 + tieEquity
    p2Win = getPer p2
    p2Equity = getPer p2 + tieEquity
    tieEquity = (getPer d) / 2


compareRangeToRange :: [[Card]] -> [Card] -> [[Card]] -> HandResults
compareRangeToRange range1 board range2 = foldl sumTuple (0,0,0) results
  where
    results = map (compareHandToRange adj2 board) adj1
    adj1    = removeDeadCards board range1
    adj2    = removeDeadCards board range2

sumTuple :: HandResults -> HandResults -> HandResults
sumTuple (r1,r2,r3) (r4,r5,r6) = (r1+r4,r2+r5,r3+r6)

compareHandToRange :: [[Card]] -> [Card] -> [Card] -> HandResults
compareHandToRange range board hand = foldl (compareTwoHands hand board) (0, 0, 0) (removeDeadCards hand range)

notInHand :: Card -> [Card] -> Bool
notInHand checkCard hand = not $ ((head hand == checkCard) || (hand !! 1 == checkCard))

notInRange :: [Card] -> [Card] -> Bool
notInRange deadCards hand = foldl (\condition h -> condition && (notInHand h hand)) True deadCards

removeDeadCards :: [Card] -> [[Card]] -> [[Card]]
removeDeadCards deadCards range = filter (notInRange deadCards) range

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
