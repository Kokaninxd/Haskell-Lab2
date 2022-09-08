module Blackjack where
import Test.QuickCheck hiding (shuffle)
import Cards
import RunGame

aCard1 :: Card
aCard1 = Card Ace Hearts

aCard2 :: Card
aCard2 = Card Ace Spades

aCard3 :: Card
aCard3 = Card (Numeric 7) Diamonds

aCard4 :: Card
aCard4 = Card Jack Clubs

aCard5 :: Card
aCard5 = Card (Numeric 3) Spades

aCard6 :: Card
aCard6 = Card (Numeric 10) Clubs

aCard7 :: Card
aCard7 = Card Queen Spades

aHand :: Hand
aHand = [aCard1, aCard6]

bHand :: Hand
bHand = [aCard3, aCard4, aCard5]

cHand :: Hand
cHand = [aCard7, aCard3, aCard1]

dHand :: Hand
dHand = [aCard7, aCard6, aCard3]





--Task A1

sizeSteps :: [Int]
sizeSteps = [ size aHand
            , size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
            , 1 + size (Card Jack Spades : [])
            , 1 + 1 + size []
            , 1 + 1 + 0
            , 2
            ]

-- Task A2

displayCard :: Card -> String
displayCard card =  (getRank (rank card)) ++ " of " ++ show (suit card) 

--getRank takes the rank of a numeric card and makes it so that it doesn't print "Numeric"
getRank :: Rank -> String
getRank (Numeric n) = show n
getRank r = show r

display :: Hand -> String
display [] = ""
display (card:[]) = displayCard card
display (card:hand) = displayCard card ++ ", " ++ display hand



valueRank :: Rank -> Int
valueRank (Numeric n) = n 
valueRank Ace = 11
valueRank _ = 10

valueCard :: Card -> Int
valueCard card = valueRank (rank card)

numberOfAces :: Hand -> Int
numberOfAces [] = 0
numberOfAces ((Card Ace _):hand) = 1 + numberOfAces hand
numberOfAces (_:hand) = numberOfAces hand

--uses value' to determine 
value :: Hand -> Int
value hand 
    |value' hand > 21 = value' hand - 10 * numberOfAces hand
    |otherwise = value' hand

--adds together the value of a hand 
value' :: Hand -> Int
value' [] = 0
value' (card:hand) = valueCard card + value' hand


gameOver :: Hand -> Bool
gameOver hand 
  |value hand > 21 = True
  |otherwise = False

--Checks if player has 
winner :: Hand -> Hand -> Player
winner player bank 
  |gameOver player = Bank
  |gameOver bank = Guest
  |value player > value bank = Guest
  |otherwise = Bank



fullDeck :: Deck 
fullDeck  = [Card (Numeric x) y | x <- [2 .. 10], y <- [Hearts, Diamonds, Spades, Clubs]] ++ 
 [Card x y | x <- [Jack, Queen, King, Ace], y <- [Hearts, Clubs, Spades, Diamonds]]


prop_size_fullDeck :: Bool
prop_size_fullDeck = size fullDeck == 52

{-

draw :: Deck -> Hand -> (Deck, Hand)
draw 

first :: (a, b) -> a
first (x, y) = x



displayDeck :: Deck -> String
displayDeck fullDeck = 

displayCard :: Card -> String
displayCard card =  (getRank (rank card)) ++ " of " ++ show (suit card) 

-}