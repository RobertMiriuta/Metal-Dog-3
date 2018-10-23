module Datatypes where


windowSizeFloat :: (Float, Float)
windowSizeFloat = (800.0, 400.0)

windowSizeInt :: (Int, Int)
windowSizeInt = (800, 400)

data MetalDogGame = Game {player::Player,
                          projectiles::[Projectile],
                          enemies :: [Enemy]} 
                           

data Gamestate = Playing
                |Paused

data Point = Pt {pX::Float, pY::Float}

data Vector = Vctr {vX::Float, vY::Float}

data Speed = Spd {speedPerTickX::Float}

--All hitboxes are rectangles
data HitBox = HBox {topLeft::Point, bottomRight::Point} 

data Projectile = Prjtl {speed :: Speed,
                          position :: Point,
                          size :: Int }
                        
data Weapon = Wpn {wDamage::Int, 
                   projectileSpeed::Speed,
                   projectileSize::HitBox,
                   rechargeTime::Int}

data Enemy = Enemy { enemyKind :: EnemyKind,
                     health::Int,
                     ePosition::Point,
                     eHitbox::HitBox, 
                     eSpeed::Speed,
                     reward::Score}

data EnemyKind = Cat
              |Postman
              |Firework
              |Car
              |VacuumCleaner

data Score = Score Int



data Player = Plyr {pPosition::Point}

class Collidable a where
  isHitBy ::Collidable b => a -> b -> Bool

class Damageable a where
  getHealth :: a -> Int
  takeDamage :: a -> Int -> Maybe a


class Moveable a where
  getPos :: a -> Point
  move :: a -> Vector -> a   
  getSpeed :: a -> Speed
  isOutOfBounds :: a -> Maybe a
