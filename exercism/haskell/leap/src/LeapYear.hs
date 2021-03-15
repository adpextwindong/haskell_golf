module LeapYear (isLeapYear) where

cleanBy n = (== 0) . (flip mod) n

isLeapYear :: Integer -> Bool
isLeapYear y = everyFour y && ((not . everyHunid) y) || (everyHunid y && everyFHunid y)
    where
        everyFour = cleanBy 4
        everyHunid = cleanBy 100
        everyFHunid = cleanBy 400
