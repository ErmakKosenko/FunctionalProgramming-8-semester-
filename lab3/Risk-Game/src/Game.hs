module Game where

import Control.Monad.Random ( Rand, StdGen )
import Battle ( battle, Battlefield(Battlefield) )

game :: Battlefield -> Rand StdGen Battlefield
game battlefield@(Battlefield attackers defenders)
  | attackers < 2 || defenders < 1 = return battlefield
  | otherwise = battle battlefield >>= game