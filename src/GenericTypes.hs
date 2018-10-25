module GenericTypes where

  data Point = Pt {xP::Float, yP::Float}

  data Vector = Vctr {uV::Float, vV::Float}

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
                  where l1x = xP (topLeft (getHitbox a))
                        l2x = xP (topLeft (getHitbox b))
                        l1y = yP (topLeft (getHitbox a))
                        l2y = yP (topLeft (getHitbox b))
                        r1x = xP (bottomRight (getHitbox a))
                        r2x = xP (bottomRight (getHitbox b))
                        r1y = yP (bottomRight (getHitbox a))
                        r2y = yP (bottomRight (getHitbox b))

  class Damageable a where
    getHealth :: a -> Int
    takeDamage :: a -> Int -> Maybe a

  --All moveable objects have a size associated with them
  --this is an extra requirement for the bounds calculations
  class Moveable a where
    getPos :: a -> Point
    getSpeed :: a -> Speed
    getSize :: a -> Point      --returns the bottom right corner pixel of the model
    move :: a -> Vector -> a   --moves model and hitbox
    isOutOfBounds :: a -> (Float, Float) -> Bool
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

  multVectorSpeed :: Vector -> Speed -> Vector
  multVectorSpeed vec speed = Vctr ((uV vec) * (speedPerTickX speed)) ((vV vec) * (speedPerTickY speed))
