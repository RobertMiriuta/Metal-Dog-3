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
      where newPosX = (xP (position p)) + (uV moveVec)
            newPosY = (yP (position p)) + (vV moveVec)
            newPos =  Pt newPosX newPosY
            newBoxXTL = (xP (topLeft (hitbox p))) + (uV moveVec)
            newBoxYTL = (yP (topLeft (hitbox p))) + (vV moveVec)
            newBoxXBR = (xP (bottomRight (hitbox p))) + (uV moveVec)
            newBoxYBR = (yP (bottomRight (hitbox p))) + (vV moveVec)
            newHitTL = Pt newBoxXTL newBoxYTL
            newHitBR = Pt newBoxXBR newBoxYBR
            newHitbox = HBox newHitTL newHitBR
            moveVec = multVectorSpeed dir (getSpeed p)
