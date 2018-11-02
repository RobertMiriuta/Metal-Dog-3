module GenericTypes where

  data Point = Pt {xP::Float, yP::Float}
      deriving (Show,Eq)

  data Vector = Vctr {uV::Float, vV::Float}
      deriving Show

  data Speed = Spd {speedPerTickX::Float, speedPerTickY::Float}
      deriving (Show,Eq)

  --All hitboxes are rectangles
  data Hitbox = HBox {topLeft::Point, bottomRight::Point}
      deriving (Show, Eq)

  data Score = Score Int
      deriving (Show, Eq)

  standardPlayerHitbox :: Hitbox
  standardPlayerHitbox = HBox (Pt 0.0 10.0) (Pt 30.0 (-10.0))

  class Mathable a where
    iAdd :: a -> a -> a
    iMult :: a -> a -> a
    iSub :: a -> a -> a
    iMultScalar :: a -> Float -> a

  class Damageable a where
    getHealth :: a -> Int
    takeDamage :: a -> Int -> Maybe a

  --All moveable objects have a size associated with them
  --this is an extra requirement for the bounds calculations
  class Moveable a where
    getPos :: a -> Point
    getSpeed :: a -> Speed
    getSize :: a -> Point      --returns the bottom right corner pixel of the hitbox
    move :: a -> Vector -> a   --moves model and hitbox
    isOutOfBounds :: a -> (Float, Float) -> Bool
    getHitbox :: a -> Hitbox
    isHitBy :: Moveable b => a -> b -> Bool
    isHitBy first second
                | topXfirst > bottomXsecond = False
                | topYfirst < bottomYsecond = False
                | bottomXfirst < topXsecond = False
                | bottomYfirst > topYsecond = False
                | otherwise = True
                  where topXfirst = xP (topLeft (getHitbox first))
                        topXsecond = xP (topLeft (getHitbox second))
                        topYfirst = yP (topLeft (getHitbox first))
                        topYsecond = yP (topLeft (getHitbox second))
                        bottomXfirst = xP (bottomRight (getHitbox first))
                        bottomXsecond = xP (bottomRight (getHitbox second))
                        bottomYfirst = yP (bottomRight (getHitbox first))
                        bottomYsecond = yP (bottomRight (getHitbox second))
    calcSpeedToPoint :: a -> Point -> Speed
    calcSpeedToPoint a pt = Spd (xP newSpeedVec) (yP newSpeedVec)
      where bigVector = iSub pt (getPos a)
            speedNorm = getSpeedVectorMagnitude (getSpeed a)
            bigNorm = getSpeedVectorMagnitude (Spd (xP bigVector) (yP bigVector))
            normalizer = speedNorm / bigNorm
            newSpeedVec = iMultScalar bigVector normalizer

  getSpeedVectorMagnitude :: Speed -> Float
  getSpeedVectorMagnitude a = sqrt (xSpd * xSpd + ySpd * ySpd)
    where xSpd = speedPerTickX  a
          ySpd = speedPerTickY  a

  multVectorSpeed :: Vector -> Speed -> Vector
  multVectorSpeed vec speed = Vctr ((uV vec) * (speedPerTickX speed)) ((vV vec) * (speedPerTickY speed))

  instance Mathable Point where
    iAdd (Pt a b) (Pt c d)      = Pt (a+c) (b+d)
    iSub (Pt a b) (Pt c d)      = Pt (a-c) (b-d)
    iMult (Pt a b) (Pt c d)     = Pt (a*c) (b*d)
    iMultScalar (Pt a b) scalar = Pt (a*scalar) (b*scalar)

  instance Mathable Vector where
    iAdd (Vctr a b) (Vctr c d)    = Vctr (a+c) (b+d)
    iSub (Vctr a b) (Vctr c d)    = Vctr (a-c) (b-d)
    iMult (Vctr a b) (Vctr c d)   = Vctr (a*c) (b*d)
    iMultScalar (Vctr a b) scalar = Vctr (a*scalar) (b*scalar)

  instance Mathable Score where
    iAdd (Score a) (Score b)     = Score (a+b)
    iSub (Score a) (Score b)     = Score (a-b)
    iMult (Score a) (Score b)    = Score (a*b)
    iMultScalar (Score a) scalar = Score (a*(round scalar))

  instance Mathable Speed where
    iAdd (Spd a b) (Spd c d)     = Spd (a+c) (b+d)
    iSub (Spd a b) (Spd c d)     = Spd (a-c) (b-d)
    iMult (Spd a b) (Spd c d)    = Spd (a*c) (b*d)
    iMultScalar (Spd a b) scalar = Spd (a*scalar) (b*scalar)
