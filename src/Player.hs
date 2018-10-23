module Player where

  import GenericTypes

  data Player = Plyr {position::Point,
                      movementSpeed::Speed,
                      hitbox::Hitbox,
                      health::Int} --activeWeapon::Weapon

  instance Collidable Player where
    getHitbox = hitbox

  instance Damageable Player where
    getHealth = health
    takeDamage p dmg | newHealth <= 0 = Nothing
                     | otherwise = Just (p {health = newHealth})
                        where newHealth = (getHealth p) - dmg

  instance Moveable Player where
    getPos = position
    getSpeed = movementSpeed
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
