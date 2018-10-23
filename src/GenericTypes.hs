
{-# LANGUAGE MultiParamTypeClasses #-}

module GenericTypes where

  data Point = Pt {x::Float, y::Float}

  data Vector = Vctr {u::Float, v::Float}

  data Speed = Spd {speedPerTickX::Float, speedPerTickY::Float}

  --All hitboxes are rectangles
  data Hitbox = HBox {topLeft::Point, bottomRight::Point}

  data Score = Score Int

  class Collidable a where
    getHitbox :: a -> Hitbox
    isHitBy :: Collidable b => a -> b -> Bool
    isHitBy a b | l1x > r2x = False
                | l2x > r1x = False
                | l1y < r2y = False
                | l2y < r1y = False
                | otherwise = True
                  where l1x = x (topLeft (getHitbox a))
                        l2x = x (topLeft (getHitbox b))
                        l1y = y (topLeft (getHitbox a))
                        l2y = y (topLeft (getHitbox b))
                        r1x = x (bottomRight (getHitbox a))
                        r2x = x (bottomRight (getHitbox b))
                        r1y = y (bottomRight (getHitbox a))
                        r2y = y (bottomRight (getHitbox b))

  class Damageable a where
    getHealth :: a -> Int
    takeDamage :: a -> Int -> Maybe a

  class Moveable a where
    getPos :: a -> Point
    getSpeed :: a -> Speed
    move :: a -> Vector -> a   --moves model and hitbox

  multVectorSpeed :: Vector -> Speed -> Vector
  multVectorSpeed vec speed = Vctr ((u vec) * (speedPerTickX speed)) ((v vec) * (speedPerTickY speed))
