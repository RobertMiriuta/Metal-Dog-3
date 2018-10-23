module Projectile where


  data Projectile = Prjtl {speed :: Speed,
                           position :: Point,
                           --damage :: Int,
                           size :: Int,
                           hitBox :: HitBox}

instance Moveable Projectile where
  getPos = position
  getSpeed = speed
  move p dir = Prjtl (speed p) newPos (size p) newHitbox
    where newPosX = x . position p + u moveVec
          newPosY = y . position p + v moveVec
          newPos =  Point newPosX newPosY
          newHitTL = Point (x . topLeft . hitbox p + u moveVec) (y . topLeft . hitbox p + v moveVec)
          newHitBR = Point (x . bottomRight . hitbox p + u moveVec) (y . bottomRight . hitbox p + v moveVec)
          newHitbox = HBox newHitTL newHitBR
          moveVec = multVectorSpeed dir (getSpeed p)
