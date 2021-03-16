module Bob (responseFor) where

import Data.Char

responseFor :: String -> String
responseFor [] = "Fine. Be that way!"
responseFor s
    | pYell alphaOnly && not (null alphaOnly) = if pQuestion s
                     then "Calm down, I know what I'm doing!"
                     else "Whoa, chill out!"
    | pSilence = "Fine. Be that way!"
    | pQuestion s = "Sure."
    | otherwise = "Whatever."
  where
    alphaOnly = filter isLetter s
    pQuestion = (=='?') . head . reverse . filter (not . isSpace)
    pYell = all isUpper
    pSilence = null s || all isSpace s
