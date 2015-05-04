module Card ( Card(..), Rank(..), Suit(..), hasPair, hasTwoPair, hasTrips, hasStraight, hasFlush, hasQuads, hasStraightFlush) where

import Data.List

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Ord, Eq)
data Suit = Spades | Diamonds | Clubs | Hearts deriving (Ord, Eq)
data Card = Card Rank Suit 

instance Show Rank where
    show Two = "2"
    show Three = "3"
    show Four = "4"
    show Five = "5"
    show Six = "6"
    show Seven = "7"
    show Eight = "8"
    show Nine = "9"
    show Ten = "10"
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
    | a == Three    = Two
    | a == Two      = Ace

getSuit :: Card -> Suit
getSuit (Card _ s) = s

getRank :: Card -> Rank
getRank (Card r _) = r

compareSuits :: Card -> Card -> Ordering
compareSuits a b = (getSuit a) `compare ` (getSuit b)

sortBySuit :: [Card] -> [Card]
sortBySuit (x:xs) = sortBy compareSuits (x:xs)
sortBySuit _      = []

--all hand functions below assume sorted input
hasPair :: [Card] -> Maybe [Card]
hasPair (x:y:xs)
    | x == y    = Just (x:y:[])
    | otherwise = hasPair (y:xs)
hasPair _       = Nothing

hasTwoPair :: [Card] -> Maybe [Card]
hasTwoPair cards = do
        firstPair <- hasPair cards
        secondPair <- hasPair $ filter (`notElem` firstPair) cards
        return (firstPair ++ secondPair)
                                      

hasTrips :: [Card] -> Maybe [Card]
hasTrips (x:y:z:xs)
    | x == y && x == z      = Just (x:y:z:[])
    | otherwise             = hasTrips (y:z:xs)
hasTrips _                  = Nothing

hasStraight :: [Card] -> [Card]
hasStraight []     = []
hasStraight (x:xs) 
--if an ace exists then a wheel is possible, so append it to the end so we can
--check for wheel straights.
    | getRank x == Ace  = hasStraightInner (x:xs ++ ace) []
    | otherwise         = hasStraightInner (x:xs) []
    --the suit of ace is irrelevant
    where ace = Card Ace (getSuit x):[]

hasStraightInner :: [Card] -> [Card] -> [Card]
hasStraightInner (x:y:ys) a
    | length a == 5                                 = a
    | (getNextLowerRank (getRank x)) == (getRank y) = hasStraightInner(y:ys) (a ++ (x:[]))
    | otherwise                                     = hasStraightInner(y:ys) []
hasStraightInner(y:ys) a
    | length a == 5     = a
    | length a == 4     = (a ++ (y:[]))
    | otherwise         = []
hasStraightInner[] _        = []

hasFlush :: [Card] -> Maybe [Card]
hasFlush (v:w:x:y:z:zs)
    | v == w && v == x && v == y && v == z  = Just (v:w:x:y:z:[])
    | otherwise                             = hasFlush (w:x:y:z:zs)
hasFlush _                                  = Nothing

hasFullHouse :: [Card] -> Maybe [Card]
hasFullHouse cards = case hasPair cards of Nothing -> Nothing
                                           Just x  -> case trips of Nothing -> Nothing
                                                                    Just y  -> Just (y ++ x)
                                                       where trips = hasTrips (filter (`notElem` x) cards)
hasQuads :: [Card] -> Maybe [Card]
hasQuads (w:x:y:z:zs)
    | w == x && w == y && w == z    = Just (w:x:y:z:[])
    | otherwise                     = hasQuads (x:y:z:zs)
hasQuads _                          = Nothing

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

hasStraightFlush :: [Card] -> [Card]
hasStraightFlush [] = []
hasStraightFlush c  = take 5 $ concat $ map hasStraight (separateBySuit c) 

