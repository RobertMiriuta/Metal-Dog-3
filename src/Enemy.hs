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
    move enmy dir = enmy {position = newPos, hitbox = newHitbox}
      where newPosX = (x (position enmy)) + (u moveVec)
            newPosY = (y (position enmy)) + (v moveVec)
            newPos =  Pt newPosX newPosY
            newBoxXTL = (x (topLeft (hitbox enmy))) + (u moveVec)
            newBoxYTL = (y (topLeft (hitbox enmy))) + (v moveVec)
            newBoxXBR = (x (bottomRight (hitbox enmy))) + (u moveVec)
            newBoxYBR = (y (bottomRight (hitbox enmy))) + (v moveVec)
            newHitTL = Pt newBoxXTL newBoxYTL
            newHitBR = Pt newBoxXBR newBoxYBR
            newHitbox = HBox newHitTL newHitBR
            moveVec = multVectorSpeed dir (getSpeed enmy)
