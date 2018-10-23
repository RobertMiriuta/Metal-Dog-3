module Enemy where

  import Datatypes

  data Enemy = Enemy { enemyKind :: EnemyKind
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

  instance Moveable Player where
    getPos = position
    getSpeed = speed
    move enmy vec = Enemy (enemyKind enmy) (health enmy) newPos newHitbox (speed enmy) (reward enmy)
      where newPosX = x . position enmy + u moveVec
            newPosY = y . position enmy + v moveVec
            newPos =  Point newPosX newPosY
            newHitTL = Point (x . topLeft . hitbox enmy + u moveVec) (y . topLeft . hitbox enmy + v moveVec)
            newHitBR = Point (x . bottomRight . hitbox enmy + u moveVec) (y . bottomRight . hitbox enmy + v moveVec)
            newHitbox = HBox newHitTL newHitBR
            moveVec = multVectorSpeed dir (getSpeed enmy)
