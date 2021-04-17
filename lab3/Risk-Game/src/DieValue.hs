module DieValue where

import Control.Arrow ( Arrow(first) )
import Control.Monad.Random ( MonadRandom(getRandom), Rand, Random(randomR, random), StdGen )

minDiceValue :: Int
minDiceValue = 1
maxDiceValue :: Int
maxDiceValue = 6

newtype DieValue = DV { unDV :: Int } deriving (Eq, Ord, Show)

instance Random DieValue where
  random           = first DV . randomR (minDiceValue, maxDiceValue)
  randomR (lo, hi) = first DV . randomR (max minDiceValue (unDV lo), min maxDiceValue (unDV hi))

die :: Rand StdGen DieValue
die = getRandom