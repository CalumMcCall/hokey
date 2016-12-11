module Hokey.Hand(hasPair, hasTwoPair, hasTrips, hasStraight, hasFlush, hasFullHouse, hasQuads, hasStraightFlush, getBestHand, Hand(..), HandType(..)) where

import Data.List
import Hokey.Card

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
getBestHand (v:w:x:y:z:zs)  = getBestHandInner (v:w:x:y:z:zs)
getBestHand _               = Nothing

getBestHandInner :: [Card] -> Maybe Hand
getBestHandInner (v:w:x:y:z:zs) = mapValue res handConstructors
        where res           = [sf, q, fh, f, s, t, tp, op]
              sf            = hasStraightFlush (v:w:x:y:z:zs)
              q             = hasQuads (v:w:x:y:z:zs)
              fh            = hasFullHouse (v:w:x:y:z:zs)
              f             = hasFlush (v:w:x:y:z:zs)
              s             = hasStraight (v:w:x:y:z:zs)
              t             = hasTrips (v:w:x:y:z:zs)
              tp            = hasTwoPair (v:w:x:y:z:zs)
              op            = hasPair (v:w:x:y:z:zs)
getBestHandInner _ = Nothing

handConstructors :: [[Card] -> Hand]
handConstructors = map Hand [StraightFlush,
                             FullHouse,
                             Flush,
                             Straight,
                             Trips,
                             TwoPair,
                             OnePair,
                             HighCard]

mapValue :: (Ord a) => [[a]] -> [([a] -> b)] -> Maybe b
mapValue (x:[]) (c:[])  = Just $ c x
mapValue (x:xs) (c:cs)
    | x /= [] = Just $ c x
    | otherwise = mapValue xs cs
mapValue _ _            = Nothing

hasPair :: [Card] -> [Card]
hasPair (v:w:x:y:z:zs)
    | pair /= []    = pair ++ (take 3 $ filter (`notElem` pair) sortedCards)
    | otherwise     = []
        where pair = hasPairInner sortedCards
              sortedCards = reverse $ sort (v:w:x:y:z:zs)
hasPair _ = []

-- assumes sorted input
hasPairInner :: [Card] -> [Card]
hasPairInner (x:y:xs)
    | x `eqRank` y = (x:y:[])
    | otherwise    = hasPairInner (y:xs)
hasPairInner _       = []

hasTwoPair :: [Card] -> [Card]
hasTwoPair (v:w:x:y:z:zs)
    | firstPair /= [] && secondPair /= []   = firstPair ++ secondPair ++ kicker
    | otherwise                             = []
        where firstPair  = hasPairInner sortedCards
              secondPair = hasPairInner $ remainder
              remainder  = filter (`notElem` firstPair) sortedCards
              kicker     = take 1 $ filter (`notElem` secondPair) remainder
              sortedCards   = reverse $ sort (v:w:x:y:z:zs)
hasTwoPair _ = []

hasTrips :: [Card] -> [Card]
hasTrips (v:w:x:y:z:zs)
    | trips /= []       = trips ++ kickers
        where trips     = hasTripsInner sortedCards
              kickers   = take 2 $ filter (`notElem` trips) sortedCards
              sortedCards   = reverse $ sort (v:w:x:y:z:zs)
hasTrips _ = []

hasTripsInner :: [Card] -> [Card]
hasTripsInner (x:y:z:xs)
    | x `eqRank` y && x `eqRank` z = (x:y:z:[])
    | otherwise                    = hasTripsInner (y:z:xs)
hasTripsInner _ = []

hasStraight :: [Card] -> [Card]
hasStraight (x:xs)
--if an ace exists then a wheel is possible, so append it to the end so we can
--check for wheel straights.
    | getRank (head sortedCards) == Ace = hasStraightInner (sortedCards ++ ace)
    | otherwise                         = hasStraightInner sortedCards
    --the suit of ace is irrelevant
    where ace = Card Ace (getSuit x):[]
          sortedCards = reverse $ sort $ (x:xs)
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
hasFlush arg = hasFlushInner $ reverse $ sort $ arg

hasFlushInner :: [Card] -> [Card]
hasFlushInner (v:w:x:y:z:zs)
    | vx == wx && vx == xx && vx == yx && vx == zx  = (v:w:x:y:z:[])
    | otherwise                                     = hasFlushInner (w:x:y:z:zs)
        where vx = getSuit v
              wx = getSuit w
              xx = getSuit x
              yx = getSuit y
              zx = getSuit z
hasFlushInner _ = []

hasFullHouse :: [Card] -> [Card]
hasFullHouse [] = []
hasFullHouse cards
    | pair /= [] && trips /= [] = trips ++ pair
    | otherwise                 = []
        where pair  = hasPairInner (filter (`notElem` trips) sortedCards)
              trips = hasTripsInner sortedCards
              sortedCards = reverse $ sort $ cards

hasQuads :: [Card] -> [Card]
hasQuads (v:w:x:y:z:zs)
    | length res == 4 = res ++ [(head $ filter (`notElem` res) sortedCards)]
    | otherwise = []
    where sortedCards = reverse $ sort (v:w:x:y:z:zs)
          res         = hasQuadsInner sortedCards
hasQuads _ = []

hasQuadsInner :: [Card] -> [Card]
hasQuadsInner (v:w:x:y:zs)
    | v `eqRank` w && w `eqRank` x && x `eqRank` y = (v:w:x:y:[])
    | otherwise                                    = hasQuadsInner (w:x:y:zs)
hasQuadsInner _ = []

hasStraightFlush :: [Card] -> [Card]
hasStraightFlush (w:x:y:z:zs) = hasStraightFlushInner $ reverse $ sort $ (w:x:y:z:zs)
hasStraightFlush _ = []

hasStraightFlushInner :: [Card] -> [Card]
hasStraightFlushInner c = take 5 $ concat $ map hasStraight (separateBySuit c)
