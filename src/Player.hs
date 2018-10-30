module Player where

  import GenericTypes

  data Player = Plyr {position::Point,
                      movementSpeed::Speed,
                      hitbox::Hitbox,
                      health::Int,
                      status::String
                      } --activeWeapon::Weapon
                      deriving Eq

  instance Damageable Player where
    getHealth = health
    takeDamage p dmg | newHealth <= 0 = Nothing
                     | otherwise = Just (p {health = newHealth})
                        where newHealth = (getHealth p) - dmg

  --every moveable object has a size and hitbox and can collide with other moveable objects
  instance Moveable Player where
    getHitbox = hitbox
    getPos = topLeft . hitbox
    getSpeed = movementSpeed
    getSize = bottomRight . hitbox
    move p dir = p {position = newPos, hitbox = newHitbox}
      where newPosX = (xP (position p)) + (uV moveVec)
            newPosY = (yP (position p)) + (vV moveVec)
            newPos =  Pt newPosX newPosY
            newBoxXTL = (xP (topLeft standardPlayerHitbox)) + newPosX
            newBoxYTL = (yP (topLeft standardPlayerHitbox)) + newPosY
            newBoxXBR = (xP (bottomRight standardPlayerHitbox)) + newPosX
            newBoxYBR = (yP (bottomRight standardPlayerHitbox)) + newPosY
            newHitTL = Pt newBoxXTL newBoxYTL
            newHitBR = Pt newBoxXBR newBoxYBR
            newHitbox = HBox newHitTL newHitBR
            moveVec = multVectorSpeed dir (getSpeed p)
    isOutOfBounds a windowSize | xSz > widthHalf = True    --out to the right
                               | ySz < -heightHalf = True   --out to the bottom
                               | xCo < -widthHalf = True   --out to the left
                               | yCo > heightHalf = True  --out to the top
                               | otherwise = False
                                 where xCo = xP (getPos a)
                                       yCo = yP (getPos a)
                                       xSz = xP (getSize a)
                                       ySz = yP (getSize a)
                                       widthHalf = (fst windowSize)/2
                                       heightHalf = (snd windowSize)/2
