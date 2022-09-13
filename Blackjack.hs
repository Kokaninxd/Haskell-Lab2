module Blackjack where

import Cards
import RunGame

aCard1 :: Card
aCard1 = Card Ace Hearts

aCard2 :: Card
aCard2 = Card Ace Spades

aCard3 :: Card
aCard3 = Card (Numeric 5) Diamonds

aCard4 :: Card
aCard4 = Card Jack Clubs

aCard5 :: Card
aCard5 = Card (Numeric 3) Spades

aCard6 :: Card
aCard6 = Card (Numeric 10) Clubs

aCard7 :: Card
aCard7 = Card Queen Spades

aHand :: Hand
aHand = [aCard3, aCard3]

bHand :: Hand
bHand = [aCard3, aCard4, aCard5]

cHand :: Hand
cHand = [aCard7, aCard3, aCard1]

dHand :: Hand
dHand = [aCard7, aCard6, aCard3]

xHand :: Card -> Hand -> Hand
xHand x y = x:y




{-
Task A1
Replicates the 'size' function line by line
-}

sizeSteps :: [Int]
sizeSteps = [ size aHand
            , size (Card (Numeric 4) Spades : (Card (Numeric 10) Spades : []))
            , 1 + size (Card (Numeric 10) Spades : [])
            , 1 + 1 + size [] 
            , 1 + 1 + 0 
            , 2     ]
---------------------------------------------------------------------------------------------
  -- Task A2
--Shows a given card in a string format
displayCard :: Card -> String
displayCard card =  (getRank (rank card)) ++ " of " ++ show (suit card) 

--getRank takes the rank of a numeric card and makes it so that it doesn't print "Numeric"
getRank :: Rank -> String
getRank (Numeric n) = show n
getRank r = show r

-- Shows all the card in a given hand in a list
display :: Hand -> String
display [] = ""
display (card:[]) = displayCard card
display (card:hand) = displayCard card ++ ", " ++ display hand
-----------------------------------------------------------------------------------------------
-- Task A3
--defines the value of ranks
valueRank :: Rank -> Int
valueRank (Numeric n) = n 
valueRank Ace = 11
valueRank _ = 10

-- Determines the value of a card based on the valueRank function
valueCard :: Card -> Int
valueCard card = valueRank (rank card)

-- Determines the number of Aces in a given hand
numberOfAces :: Hand -> Int
numberOfAces [] = 0
numberOfAces ((Card Ace _):hand) = 1 + numberOfAces hand
numberOfAces (_:hand) = numberOfAces hand

--uses value' and numberOfAces to determine the value of a hand incase the hand contains any Aces.
--if the hand value exceeds 21 and the hand contains Aces, the value of all Aces becomes 1.
value :: Hand -> Int
value hand 
    |value' hand > 21 = value' hand - 10 * numberOfAces hand
    |otherwise = value' hand

--adds together the value of a hand 
value' :: Hand -> Int
value' [] = 0
value' (card:hand) = valueCard card + value' hand
---------------------------------------------------------------------------------------------------
-- Task A4
--Checks if a hand value exceeds 21, if True the hand is busted and the Player loses
gameOver :: Hand -> Bool
gameOver hand 
  |value hand > 21 = True
  |otherwise = False

--Checks if Guest has busted, if true bank wins then checks the other way arround. 
--Finally it determines the winner based on hand value with the bank winning incase value Player == value Bank
winner :: Hand -> Hand -> Player
winner player bank 
  |gameOver player = Bank
  |gameOver bank = Guest
  |value player > value bank = Guest
  |otherwise = Bank

-- Task B1

{-
Creates a full deck of cards in numeric order
-}

fullDeck :: Deck 
fullDeck  = [Card (Numeric x) y | x <- [2 .. 10], y <- [Hearts, Diamonds, Spades, Clubs]] ++ 
 [Card x y | x <- [Jack, Queen, King, Ace], y <- [Hearts, Clubs, Spades, Diamonds]]

{-
Checks wether a deck is full by containing 52 cards
-}
prop_size_fullDeck :: Bool
prop_size_fullDeck = size fullDeck == 52

-- Task B2

{-
Function that draws the top card of the input deck and adds it into the input hand.
-}

draw :: Deck -> Hand -> (Deck, Hand)
draw [] hand = error "draw: The deck is empty."
draw deck hand = (tail(deck), [head(deck)] ++ hand)

{-
Simulates the Bank in Blackjack by drawing cards from a deck and adding them into an input hand
until the value of the hand is more or equal to 16.
-}

--Task B3
playBank :: Deck -> Hand 
playBank deck 
  | deck == [] = error "Empty deck!"
  | otherwise = playBank' deck []


playBank' deck bankHand
  | value bankHand >= 16 = bankHand
  | otherwise = playBank' deck' bankHand'
  where (deck', bankHand') = draw deck bankHand

-- Task B4
newDeck = []

shuffle :: [Double] -> Deck -> Deck
shuffle list deck
  | deck == []           = newDeck
  | list == []           = newDeck
  | (head list) > 0.5    = (newDeck ++ [head deck]) ++ shuffle list' deck'
  | otherwise            = shuffle list' deck'
  where list'            = tail list 
        deck'            = tail deck


{-
first :: (a, b) -> a
first (x, y) = x



displayDeck :: Deck -> String
displayDeck fullDeck = 

displayCard :: Card -> String
displayCard card =  (getRank (rank card)) ++ " of " ++ show (suit card) 
x-}