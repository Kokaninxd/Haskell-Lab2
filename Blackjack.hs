module Blackjack where

import Cards
import RunGame



aCard1 :: Card
aCard1 = Card (Numeric 3) Hearts

aCard2 :: Card
aCard2 = Card Jack Hearts 


aHand :: Hand
aHand = aCard1 : aCard2 : []
{-

size hand2
  = size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
  = ...
  = 2

sizeSteps :: [Int]
sizeSteps = [ size hand2
            , size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
            , ... -- add the remaining steps here
            , 2
            ]

            -}