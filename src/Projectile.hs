-- projectile contains the type class definitions for projectiles, mainly the
-- Moveable class
module Projectile where

  import GenericTypes

  data Projectile = Prjtl {speed :: Speed,
                           position :: Point,
                           --damage :: Int,
                           size :: Int,
                           hitbox :: Hitbox,
                           age :: Float
                           }

  instance Moveable Projectile where
    getPos = topLeft . hitbox
    getSize = bottomRight . hitbox
    getHitbox = hitbox
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
    isOutOfBounds a windowSize | xCo > widthHalf = True    --out to the right
                               | yCo < -heightHalf = True   --out to the bottom
                               | xSz < -widthHalf = True   --out to the left
                               | ySz > heightHalf = True  --out to the top
                               | otherwise = False
                                 where xCo = xP (getPos a)
                                       yCo = yP (getPos a)
                                       xSz = xP (getSize a)
                                       ySz = yP (getSize a)
                                       widthHalf = (fst windowSize)/2
                                       heightHalf = (snd windowSize)/2
