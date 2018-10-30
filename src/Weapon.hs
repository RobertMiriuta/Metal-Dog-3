module Weapon where

  import Projectile
  import GenericTypes

  data Weapon = Wpn {createProjectile::(Point -> Projectile),
                     passedTime :: Float,
                     rechargeTime::Float}

  instance Eq Weapon where
    w1 == w2 = rechargeTime w1 == rechargeTime w2