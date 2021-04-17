module Battle where

import Data.List ( sortBy )
import Control.Monad.Random( replicateM, MonadRandom(getRandom), Rand, StdGen )

import DieValue ( DieValue )

type Army = Int
data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving (Show)


attackingUnitsForBattle :: Army -> Army
attackingUnitsForBattle units = min 3 (units - 1)

defendingUnitsForBattle :: Army -> Army
defendingUnitsForBattle = min 2

sortedUnitPoints :: Army -> Rand StdGen [DieValue]
sortedUnitPoints nUnits = sortBy (flip compare) <$> replicateM nUnits getRandom

singleFight :: Battlefield -> (DieValue, DieValue) -> Battlefield
singleFight (Battlefield attackers defenders) (attackingUnitPoints, defendingUnitPoints)
  | attackingUnitPoints > defendingUnitPoints = Battlefield attackers (defenders - 1)
  | otherwise = Battlefield (attackers - 1) defenders

battle :: Battlefield -> Rand StdGen Battlefield
battle battlefield@(Battlefield attackers defenders) =
  do
    attackingUnitPoints <- sortedUnitPoints (attackingUnitsForBattle attackers)
    defendingUnitPoints <- sortedUnitPoints (defendingUnitsForBattle defenders)
    return $ foldl singleFight battlefield (zip attackingUnitPoints defendingUnitPoints)