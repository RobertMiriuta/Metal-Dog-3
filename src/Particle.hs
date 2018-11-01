module Particle where

import GenericTypes

data Particle = Prtcl {
                        age :: Float,
                        lifespan :: Float,
                        position :: Point,
                        alive :: Bool
                      }

growParticle :: Float -> Particle -> Particle
growParticle time particle
  | newTime > lifeSpan = deadParticle
  | otherwise          = updatedParticle
    where oldTime = age particle
          lifeSpan = lifespan particle
          newTime = time + oldTime
          updatedParticle = particle {age = newTime}
          deadParticle = particle {age = newTime, alive = False}

cleanUpParticles :: [Particle] -> [Particle]
cleanUpParticles [] = []
cleanUpParticles (particle:particles)
  |isAlive   = particle : cleanUpParticles particles
  |otherwise = cleanUpParticles particles
    where isAlive = alive particle