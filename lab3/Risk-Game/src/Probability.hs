module Probability where

import Control.Monad.Random ( replicateM, Rand, StdGen )
import Battle ( Battlefield(defenders) )
import Game ( game )

invasionSuccessProbability :: Battlefield -> Int -> Rand StdGen Double
invasionSuccessProbability battlefield nTimes = do
  battles <- replicateM nTimes (game battlefield)
  return $ fromIntegral(length (filter ((< 1) . defenders) battles)) / fromIntegral nTimes