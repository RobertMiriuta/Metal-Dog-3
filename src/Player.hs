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

instance Moveable Player where
  getPos = position
  getSpeed = movementSpeed
  move p dir = Plyr newPos (movementSpeed p) newHitbox (health p)
    where newPosX = x . position p + u moveVec
          newPosY = y . position p + v moveVec
          newPos =  Point newPosX newPosY
          newHitTL = Point (x . topLeft . hitbox p + u moveVec) (y . topLeft . hitbox p + v moveVec)
          newHitBR = Point (x . bottomRight . hitbox p + u moveVec) (y . bottomRight . hitbox p + v moveVec)
          newHitbox = HBox newHitTL newHitBR
          moveVec = multVectorSpeed dir (getSpeed p)
