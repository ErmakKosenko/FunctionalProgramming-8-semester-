module Main where

import Control.Monad.Random ( evalRandIO )

import Battle ( Battlefield(Battlefield) )
import Game ( game )
import Probability ( invasionSuccessProbability )

main :: IO ()
main = do
    let battlefield = Battlefield 10 10
    let nTimes = 10 ^ 4
    game <- evalRandIO $ game battlefield
    probability <- evalRandIO $ invasionSuccessProbability battlefield nTimes
    print "Start battlefield: "
    print battlefield
    print "End battlefield: "
    print game
    print "Invasion Success Probability: "
    print probability