module Datatypes where

data MetalDogGame = Game {player::Player,
                          projectiles::[Projectile],
                          enemies :: [Enemy],
                          --currentScore::Score,
                          --highscore::Score
                         }

data Gamestate = Playing
                |Paused

data Point = Pt {pX::Float, pY::Float}

data Vector = Vctr {u::Float, v::Float}

data Speed = Spd {speedPerTickX::Float}

--All hitboxes are rectangles
data HitBox = HBox {topLeft::Point, bottomRight::Point}

data Projectile = Prjtl {speed :: Speed,
                         position :: Point,
                         --damage :: Int,
                         size :: Int,
                         hitBox :: HitBox}

data Weapon = Wpn {projectile::Projectile
                   rechargeTime::Int}

data Score = Score Int

class Collidable a b where
  getHitbox :: a -> HitBox
  isHitBy :: a -> b -> Bool
  isHitBy a b | l1x > r2x = False
              | l2x > r1x = False
              | l1y < r2y = False
              | l2y < r1y = False
              | otherwise = True
                where l1x = x . topLeft . getHitbox a
                      l2x = x . topLeft . getHitbox b
                      l1y = y . topLeft . getHitbox a
                      l2y = y . topLeft . getHitbox b
                      r1x = x . bottomRight . getHitbox a
                      r2x = x . bottomRight . getHitbox b
                      r1y = y . bottomRight . getHitbox a
                      r2y = y . bottomRight . getHitbox b

class Damageable a where
  getHealth :: a -> Int
  takeDamage :: a -> Int -> Maybe a
  takeDamage p dmg | newHealth <= 0 = Nothing
                   | otherwise = Just newHealth
                      where newHealth = (getHealth p) - dmg

class Moveable a where
  getPos :: a -> Point
  move :: a -> Vector -> a   --moves model and hitbox
  getSpeed :: a -> Speed

multVectorSpeed :: Vector -> Speed -> Vector
multVectorSpeed vec speed = Vctr ((u vec) * (speedPerTickX speed)) ((v vec) * (speedPerTickY speed))
