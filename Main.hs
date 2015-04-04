data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace 
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
    show (Card y x) = (show y) ++ (show x)

getSuit :: Card -> Suit
getSuit (Card _ s) = s

getRank :: Card -> Rank
getRank (Card r _) = r
