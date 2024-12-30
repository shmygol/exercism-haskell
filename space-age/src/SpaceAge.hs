module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn Mercury seconds = seconds / 7600543.81992
ageOn Venus seconds = seconds / 19414149.052176
ageOn Earth seconds = seconds / 31557600.0
ageOn Mars seconds = seconds / 59354032.69008
ageOn Jupiter seconds = seconds / 374355659.124
ageOn Saturn seconds = seconds / 929292362.8848
ageOn Uranus seconds = seconds / 2651370019.3296
ageOn Neptune seconds = seconds / 5200418560.032
