import Data.List

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Ord, Eq)
data Suit = Spades | Diamonds | Clubs | Hearts 
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

getSuit :: Card -> Suit
getSuit (Card _ s) = s

getRank :: Card -> Rank
getRank (Card r _) = r

hasPair :: [Card] -> Maybe [Card]
hasPair (x:y:xs)
    | x == y    = Just (x:y:[])
    | otherwise = hasPair (y:xs)
hasPair _       = Nothing

hasTrips :: [Card] -> Maybe [Card]
hasTrips (x:y:z:xs)
    | x == y && x == z  = Just (x:y:z:[])
    | otherwise             = hasTrips (y:z:xs)
hasTrips _                  = Nothing

hasQuads :: [Card] -> Maybe [Card]
hasQuads (w:x:y:z:zs)
    | w == x && w == y && w == z    = Just (w:x:y:z:[])
    | otherwise                     = hasQuads (x:y:z:zs)
hasQuads _                          = Nothing

hasFullHouse :: [Card] -> Maybe [Card]
hasFullHouse cards = case hasPair cards of Nothing  -> Nothing
                                           Just x   -> case trips of Nothing    -> Nothing
                                                                     Just y     -> Just (y ++ x)
                                                       where trips = hasTrips (filter (`notElem` x) cards)
