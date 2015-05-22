module Card (Card(..), Rank(..), Suit(..), getNextLowerRank, getSuit, getRank, compareSuits, sortBySuit, separateBySuit, getCardsOfSuit) where

import Data.List

data Rank = Deuce | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Ord, Eq)
data Suit = Diamonds | Clubs | Hearts | Spades deriving (Ord, Eq)
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
    show Spades = "s"
    show Diamonds = "d"
    show Clubs = "c"
    show Hearts = "h"

instance Show Card where
    show (Card r s) = (show r) ++ (show s)

instance Ord Card where
    (Card r1 s1) `compare` (Card r2 s2) = r1 `compare` r2

instance Eq Card where
    (Card r1 s1) == (Card r2 s2) = r1 == r2

getNextLowerRank :: Rank -> Rank
getNextLowerRank a
    | a == Ace      = King
    | a == King     = Queen
    | a == Queen    = Jack
    | a == Jack     = Ten
    | a == Ten      = Nine
    | a == Nine     = Eight
    | a == Eight    = Seven
    | a == Seven    = Six
    | a == Six      = Five
    | a == Five     = Four
    | a == Four     = Three
    | a == Three    = Deuce
    | a == Deuce      = Ace

getSuit :: Card -> Suit
getSuit (Card _ s) = s

getRank :: Card -> Rank
getRank (Card r _) = r

compareSuits :: Card -> Card -> Ordering
compareSuits a b = (getSuit a) `compare ` (getSuit b)

sortBySuit :: [Card] -> [Card]
sortBySuit (x:xs) = sortBy compareSuits (x:xs)
sortBySuit _      = []

separateBySuit :: [Card] -> [[Card]]
separateBySuit [] = []
separateBySuit c = [spades, clubs, diamonds, hearts]
    where spades    = getCardsOfSuit Spades c
          clubs     = getCardsOfSuit Clubs c
          diamonds  = getCardsOfSuit Diamonds c
          hearts    = getCardsOfSuit Hearts c

getCardsOfSuit :: Suit -> [Card] -> [Card]
getCardsOfSuit _ [] = []
getCardsOfSuit s c  =  getCardsOfSuitInner s [] c 

getCardsOfSuitInner :: Suit -> [Card] -> [Card] -> [Card]
getCardsOfSuitInner _ l []     = l
getCardsOfSuitInner s l (x:xs)
    | getSuit x == s    = l ++ (x:[]) ++ rest
    | otherwise         = l ++ rest
             where rest = getCardsOfSuitInner s l xs

