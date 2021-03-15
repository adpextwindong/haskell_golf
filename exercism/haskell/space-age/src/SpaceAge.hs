module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

earthSeconds = 31557600

ageOn :: Planet -> Float -> Float
ageOn Mercury   = (/ (0.2408467 * earthSeconds))
ageOn Venus     = (/ (0.61519726 * earthSeconds))
ageOn Earth     = (/ (1.0 * earthSeconds))
ageOn Mars      = (/ (1.8808158 * earthSeconds))
ageOn Jupiter   = (/ (11.862615 * earthSeconds))
ageOn Saturn    = (/ (29.447498 * earthSeconds))
ageOn Uranus    = (/ (84.016846 * earthSeconds))
ageOn Neptune   = (/ (164.79132 * earthSeconds))
