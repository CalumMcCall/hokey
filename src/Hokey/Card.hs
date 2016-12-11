module Hokey.Card (
    Card(..),
    Rank(..),
    Suit(..),
    getNextLowerRank,
    getSuit,
    getRank,
    compareSuits,
    sortBySuit,
    separateBySuit,
    getCardsOfSuit,
    eqRank,
    ) where

import           Data.List

data Rank = Deuce
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King
          | Ace
  deriving (Ord, Eq, Enum)

data Suit = C
          | D
          | H
          | S
  deriving (Ord, Eq, Enum)

data Card = Card Rank Suit

instance Show Rank where
  show Deuce = "2"
  show Three = "3"
  show Four = "4"
  show Five = "5"
  show Six = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine = "9"
  show Ten = "T"
  show Jack = "J"
  show Queen = "Q"
  show King = "K"
  show Ace = "A"

instance Show Suit where
  show S = "s"
  show D = "d"
  show C = "c"
  show H = "h"

instance Show Card where
  show (Card r s) = (show r) ++ (show s)

instance Ord Card where
  (Card r1 _) `compare` (Card r2 _) = r1 `compare` r2

instance Eq Card where
  (Card r1 s1) == (Card r2 s2) = r1 == r2 && s1 == s2

eqRank :: Card -> Card -> Bool
eqRank (Card r1 _) (Card r2 _) = r1 == r2

getNextLowerRank :: Rank -> Rank
getNextLowerRank Ace = King
getNextLowerRank King = Queen
getNextLowerRank Queen = Jack
getNextLowerRank Jack = Ten
getNextLowerRank Ten = Nine
getNextLowerRank Nine = Eight
getNextLowerRank Eight = Seven
getNextLowerRank Seven = Six
getNextLowerRank Six = Five
getNextLowerRank Five = Four
getNextLowerRank Four = Three
getNextLowerRank Three = Deuce
getNextLowerRank Deuce = Ace

getSuit :: Card -> Suit
getSuit (Card _ s) = s

getRank :: Card -> Rank
getRank (Card r _) = r

compareSuits :: Card -> Card -> Ordering
compareSuits a b = (getSuit a) `compare` (getSuit b)

sortBySuit :: [Card] -> [Card]
sortBySuit (x:xs) = sortBy compareSuits (x : xs)
sortBySuit _ = []

separateBySuit :: [Card] -> [[Card]]
separateBySuit [] = []
separateBySuit c = [spades, clubs, diamonds, hearts]
  where
    spades = getCardsOfSuit S c
    clubs = getCardsOfSuit C c
    diamonds = getCardsOfSuit D c
    hearts = getCardsOfSuit H c

getCardsOfSuit :: Suit -> [Card] -> [Card]
getCardsOfSuit _ [] = []
getCardsOfSuit s c = getCardsOfSuitInner s [] c

getCardsOfSuitInner :: Suit -> [Card] -> [Card] -> [Card]
getCardsOfSuitInner _ l [] = l
getCardsOfSuitInner s l (x:xs)
  | getSuit x == s = l ++ (x : []) ++ rest
  | otherwise = l ++ rest
  where
    rest = getCardsOfSuitInner s l xs
