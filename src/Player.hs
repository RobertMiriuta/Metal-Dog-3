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
