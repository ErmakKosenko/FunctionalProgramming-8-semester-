module Main (main) where

import Test.Hspec ( hspec, describe, it, shouldBe )
import Test.QuickCheck ( Testable(property) )
import Control.Monad.Random ( evalRandIO )
import System.IO.Unsafe ( unsafePerformIO )

import Battle ( Battlefield(..), Army, battle, attackingUnitsForBattle, defendingUnitsForBattle )
import Game ( game )
import Probability ( invasionSuccessProbability )

main :: IO ()
main = hspec $ describe "Tests: " $ do
    describe "Test battle #1" $
      it "Units For Battle" $
        attackingUnitsForBattle 3 == 2 &&
        attackingUnitsForBattle 4 == 3 &&
        attackingUnitsForBattle 5 == 3 &&
        defendingUnitsForBattle 1 == 1 &&        
        defendingUnitsForBattle 2 == 2 &&
        defendingUnitsForBattle 3 == 2

    describe "Test battle #2" $
      it "Killed units" $
        property $ do
          let a = 5 
          let d = 10
          checkBattleKilledUnits (a, d) (value $ battle (Battlefield a d))

    describe "Test battle #3" $
      it "Killed units" $
        property $ do
          let a = 10 
          let d = 5
          checkBattleKilledUnits (a, d) (value $ battle (Battlefield a d))

    describe "Test battle #4" $
      it "Killed units" $
        property $ do
          let a = 10 
          let d = 10
          checkBattleKilledUnits (a, d) (value $ battle (Battlefield a d))

    describe "Test Game #1" $
      it "The whole army is dead " $ 
        property $ checkGameKilledUnits $ value $ game (Battlefield 5 10)

    describe "Test Game #2" $
      it "The whole army is dead " $ 
        property $ checkGameKilledUnits $ value $ game (Battlefield 10 5)
    
    describe "Test Game #3" $
      it "The whole army is dead " $ 
        property $ checkGameKilledUnits $ value $ game (Battlefield 10 10)

    describe "Test probability #1" $
      it "Defending army win" $
        value (invasionSuccessProbability (Battlefield 0 10) 1000) `shouldBe` (0.0 :: Double)

    describe "Test probability #2" $
      it "Defending army win" $
        value (invasionSuccessProbability (Battlefield 2 10) 1000) `shouldBe` (0.0 :: Double)

    describe "Test probability #3" $
      it "0 < probability < 1" $
        property $ do
          let correctProbability value = value > 0.0 && value < 1.0
          correctProbability (value (invasionSuccessProbability (Battlefield 10 10) 1000))

-- test utils
value = unsafePerformIO . evalRandIO

checkBattleKilledUnits :: (Army, Army) -> Battlefield -> Bool
checkBattleKilledUnits (a, d) battlefield = attackers battlefield < a || defenders battlefield < d

checkGameKilledUnits :: Battlefield -> Bool
checkGameKilledUnits battlefield = attackers battlefield == 1 || defenders battlefield == 0