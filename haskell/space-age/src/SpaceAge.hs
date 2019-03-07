module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune
            deriving Eq

yearOnEarth :: Float
yearOnEarth = 31557600.0

secsInYear :: Planet -> Float
secsInYear planet = case planet   of 
                         Mercury  -> yearOnEarth * 0.2408467
                         Venus    -> yearOnEarth * 0.61519726
                         Mars     -> yearOnEarth * 1.8808158 
                         Jupiter  -> yearOnEarth * 11.862615
                         Saturn   -> yearOnEarth * 29.447498
                         Uranus   -> yearOnEarth * 84.016846
                         Neptune  -> yearOnEarth * 164.79132
                         _        -> yearOnEarth

ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / secsInYear planet
