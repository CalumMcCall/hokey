module Hokey.Odds(HoleCards) where

import Hokey.Card

type HoleCards = (Card, Card)

--takes two player ranges and a board and returns the number of times
--player 1 wins and player 2 wins
getOdds :: [HoleCards] -> [HoleCards] -> [Card] -> (Integer, Integer)
getOdds p1 p2 boardCards = (0, 0)

ranks :: [Rank]
ranks = [Deuce .. Ace]

cons :: Suit -> Rank -> Card
cons s r = Card r s

deck :: [Card]
deck = (map (cons D) ranks) ++ (map (cons H) ranks) ++ (map (cons S) ranks) ++ (map (cons C) ranks)

remainingCards :: [Card] -> [Card]
remainingCards c = filter (`notElem` c) deck

