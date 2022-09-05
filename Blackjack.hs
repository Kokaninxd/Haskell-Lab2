module Blackjack where

import Cards
import RunGame

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