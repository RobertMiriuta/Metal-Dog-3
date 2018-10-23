module Weapon where

  import Projectile

  data Weapon = Wpn {projectile::Projectile
                     rechargeTime::Int}
