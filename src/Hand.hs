module Hand(hasPair, hasTwoPair, hasTrips, hasStraight, hasFlush, hasFullHouse, hasQuads, hasStraightFlush) where

import Data.List
import Card

data HandType = HighCard | OnePair | TwoPair | Trips | Straight | Flush | FullHouse | StraightFlush deriving (Ord, Eq)

data Hand = Hand HandType [Card]

instance Eq Hand where
    (Hand h1 l1) == (Hand h2 l2)
        | h1 == h2  = l1 == l2
        | otherwise = False

instance Ord Hand where
    (Hand h1 l1) `compare` (Hand h2 l2)
        | h1 == h2  = l1 `compare` l2
        | otherwise = h1 `compare` h2

getBestHand :: [Card] -> Maybe Hand
getBestHand (v:w:x:y:z:zs)  = getBestHandInner sortedCards
          where sortedCards = reverse $ sort (v:w:x:y:z:zs)
getBestHand _               = Nothing

--This function assumes reversed, sorted input
getBestHandInner :: [Card] -> Maybe Hand
getBestHandInner (v:w:x:y:z:zs) = mapValue res handConstructors
        where res           = [sf, q, fh, f, s, t, tp, op]
              sf            = hasStraightFlush sortedCards
              q             = hasQuads sortedCards
              fh            = hasFullHouse sortedCards
              f             = hasFlush sortedCards
              s             = hasStraight sortedCards
              t             = hasTrips sortedCards
              tp            = hasTwoPair sortedCards
              op            = hasPair sortedCards
              sortedCards   = reverse $ sort (v:w:x:y:z:zs)
getBestHandInner [] = Nothing

handConstructors = [Hand StraightFlush, Hand FullHouse, Hand Flush, Hand Straight, Hand Trips, Hand TwoPair, Hand OnePair, Hand HighCard]

mapValue :: (Ord a) => [[a]] -> [([a] -> b)] -> Maybe b
mapValue (x:[]) (c:[])  = Just $ c x
mapValue (x:xs) (c:cs)
    | x /= [] = Just $ c x
    | otherwise = mapValue xs cs
mapValue _ _            = Nothing

--all hand functions below assume reversed, sorted input
hasPair :: [Card] -> [Card]
hasPair (v:w:x:y:z:zs)
    | pair /= []    = pair ++ (take 3 $ filter (`notElem` pair) (v:w:x:y:z:zs))
    | otherwise     = []
        where pair = hasPairInner (v:w:x:y:z:zs)
hasPair _ = []

hasPairInner :: [Card] -> [Card]
hasPairInner (x:y:xs)
    | x == y    = (x:y:[])
    | otherwise = hasPairInner (y:xs)
hasPairInner _       = []

hasTwoPair :: [Card] -> [Card]
hasTwoPair (v:w:x:y:z:zs)
    | firstPair /= [] && secondPair /= []   = firstPair ++ secondPair ++ kicker
    | otherwise                             = []
        where firstPair  = hasPairInner (v:w:x:y:z:zs)
              secondPair = hasPairInner $ remainder
              remainder  = filter (`notElem` firstPair) (v:w:x:y:z:zs)
              kicker     = take 1 $ filter (`notElem` secondPair) remainder
hasTwoPair _ = []

hasTrips :: [Card] -> [Card]
hasTrips (v:w:x:y:z:zs)
    | trips /= []       = trips ++ kickers
        where trips     = hasTripsInner (v:w:x:y:z:zs)
              kickers   = take 2 $ filter (`notElem` trips) (v:w:x:y:z:zs)
hasTrips _ = []

hasTripsInner :: [Card] -> [Card]
hasTripsInner (x:y:z:xs)
    | x == y && x == z      = (x:y:z:[])
    | otherwise             = hasTripsInner (y:z:xs)
hasTripsInner _             = []

hasStraight :: [Card] -> [Card]
hasStraight (x:xs) 
--if an ace exists then a wheel is possible, so append it to the end so we can
--check for wheel straights.
    | getRank x == Ace  = hasStraightInner (x:xs ++ ace)
    | otherwise         = hasStraightInner (x:xs)
    --the suit of ace is irrelevant
    where ace = Card Ace (getSuit x):[]
hasStraight _ = []

hasStraightInner :: [Card] -> [Card]
hasStraightInner (v:w:x:y:z:zs)
    | getNextLowerRank vr == wr &&
      getNextLowerRank wr == xr &&
      getNextLowerRank xr == yr &&
      getNextLowerRank yr == zr = (v:w:x:y:z:[])
    | otherwise                 = hasStraightInner (w:x:y:z:zs)
        where vr = getRank v
              wr = getRank w
              xr = getRank x
              yr = getRank y
              zr = getRank z
hasStraightInner _ = []

hasFlush :: [Card] -> [Card]
hasFlush (v:w:x:y:z:zs)
    | vx == wx && vx == xx && vx == yx && vx == zx  = (v:w:x:y:z:[])
    | otherwise                             = hasFlush (w:x:y:z:zs)
        where vx = getSuit v
              wx = getSuit w
              xx = getSuit x
              yx = getSuit y
              zx = getSuit z
hasFlush _                                  = []

hasFullHouse :: [Card] -> [Card]
hasFullHouse []    = []
hasFullHouse cards
    | pair /= [] && trips /= [] = trips ++ pair
    | otherwise                 = []
        where pair  = hasPairInner cards
              trips = hasTripsInner (filter (`notElem` pair) cards)

hasQuads :: [Card] -> [Card]
hasQuads (w:x:y:z:zs)
    | w == x && w == y && w == z    = (w:x:y:z:[])
    | otherwise                     = hasQuads (x:y:z:zs)
hasQuads _                          = []

hasStraightFlush :: [Card] -> [Card]
hasStraightFlush [] = []
hasStraightFlush c  = take 5 $ concat $ map hasStraight (separateBySuit c) 
