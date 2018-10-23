module Projectile where

  import GenericTypes

  data Projectile = Prjtl {speed :: Speed,
                           position :: Point,
                           --damage :: Int,
                           size :: Int,
                           hitbox :: Hitbox}

  instance Moveable Projectile where
    getPos = position
    getSpeed = speed
    move p dir = p {position = newPos, hitbox = newHitbox}
      where newPosX = (x (position p)) + (u moveVec)
            newPosY = (y (position p)) + (v moveVec)
            newPos =  Pt newPosX newPosY
            newBoxXTL = (x (topLeft (hitbox p))) + (u moveVec)
            newBoxYTL = (y (topLeft (hitbox p))) + (v moveVec)
            newBoxXBR = (x (bottomRight (hitbox p))) + (u moveVec)
            newBoxYBR = (y (bottomRight (hitbox p))) + (v moveVec)
            newHitTL = Pt newBoxXTL newBoxYTL
            newHitBR = Pt newBoxXBR newBoxYBR
            newHitbox = HBox newHitTL newHitBR
            moveVec = multVectorSpeed dir (getSpeed p)
