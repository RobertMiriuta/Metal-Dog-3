module Player where

  import Datatypes

  data Player = Plyr {position::Point
                      movementSpeed::Speed,
                      hitbox::HitBox
                      health::Int
                      --activeWeapon::Weapon
                      }

instance Collidable Player where
  getHitbox = hitbox

instance Damageable Player where
  getHealth = health
  takeDamage p dmg | newHealth <= 0 = Nothing
                   | otherwise = Just newHealth
                      where newHealth = (getHealth p) - dmg

instance Moveable Player where
  getPos = position
  move p vec = Plyr(newPos, movementSpeed p, newHitbox, health p)
    where newPosX = x . position p + u vec
          newPosY = y . position p + v vec
          newPos =  Point newPosX newPosY
          newHitTL = Point (x . topLeft . hitbox p + u vec) (y . topLeft . hitbox p + v vec)
          newHitBR = Point (x . bottomRight . hitbox p + u vec) (y . bottomRight . hitbox p + v vec)q
          newHitbox = HBox newHitTL newHitBR
  getSpeed = movementSpeed
