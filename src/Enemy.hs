module Enemy where

  import GenericTypes

  data Enemy = Enemy { enemyKind :: EnemyKind,
                       health::Int,
                       position::Point,
                       hitbox::Hitbox,
                       speed::Speed,
                       reward::Score}

  data EnemyKind = Cat
                |Postman
                |Firework
                |Car
                |VacuumCleaner

  instance Collidable Enemy where
    getHitbox = hitbox

  instance Damageable Enemy where
    getHealth = health
    takeDamage enmy dmg | newHealth <= 0 = Nothing
                        | otherwise = Just (enmy {health = newHealth})
                        where newHealth = (getHealth enmy) - dmg

  instance Moveable Enemy where
    getPos = position
    getSpeed = speed
    getSize = bottomRight . hitbox
    move enmy dir = enmy {position = newPos, hitbox = newHitbox}
      where newPosX = (xP (position enmy)) + (uV moveVec)
            newPosY = (yP (position enmy)) + (vV moveVec)
            newPos =  Pt newPosX newPosY
            newBoxXTL = (xP (topLeft (hitbox enmy))) + (uV moveVec)
            newBoxYTL = (yP (topLeft (hitbox enmy))) + (vV moveVec)
            newBoxXBR = (xP (bottomRight (hitbox enmy))) + (uV moveVec)
            newBoxYBR = (yP (bottomRight (hitbox enmy))) + (vV moveVec)
            newHitTL = Pt newBoxXTL newBoxYTL
            newHitBR = Pt newBoxXBR newBoxYBR
            newHitbox = HBox newHitTL newHitBR
            moveVec = multVectorSpeed dir (getSpeed enmy)
