module Hokey.Odds (HoleCards, deck, remainingCards, compareHandToRange, compareRangeToRange, printEquity) where

import           Hokey.Card
import           Hokey.Hand

type HoleCards = (Card, Card)

--(Player 1 wins, Draw, Player 2 wins)
type HandResults = (Integer, Integer, Integer)

percent :: Double
percent = 100

printEquity :: HandResults -> IO ()
printEquity (p1,d,p2) = do
          let totalHands = p1 + d + p2
          let getPer x = fromIntegral x / fromIntegral totalHands * percent
          putStrLn $ "Player 1 win " ++ (show $ (getPer (p1+d)))
          putStrLn $ "Player 1 equity " ++ (show $ ((getPer (p1+d)) + (getPer d)))
          putStrLn $ "Player 2 win " ++ (show $ getPer (p2))
          putStrLn $ "Player 2 equity " ++ (show $ ((getPer (p2)) + (getPer d)))
          putStrLn $ "Tie equity " ++ (show $ getPer d)

compareRangeToRange :: [[Card]] -> [Card] -> [[Card]] -> HandResults
compareRangeToRange range1 board range2 = foldl sumTuple (0,0,0) results
  where
    results = map (compareHandToRange range2 board) range1

sumTuple :: HandResults -> HandResults -> HandResults
sumTuple (r1,r2,r3) (r4,r5,r6) = (r1+r4,r2+r5,r3+r6)

compareHandToRange :: [[Card]] -> [Card] -> [Card] -> HandResults
compareHandToRange range board hand = foldl (compareTwoHands hand board) (0, 0, 0) fRange
  where
    fRange = filter (\x -> (head hand) `notElem` x && (hand !! 1) `notElem` x) range

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
