module Bob (responseFor) where

import Data.Char

responseFor :: String -> String
responseFor s
    | pSilence = "Fine. Be that way!"
    | pYell alphaOnly && not (null alphaOnly) = if pQuestion s
                     then "Calm down, I know what I'm doing!"
                     else "Whoa, chill out!"
    | pQuestion s = "Sure."
    | otherwise = "Whatever."
  where
    alphaOnly = filter isLetter s
    pQuestion = (=='?') . head . reverse . filter (not . isSpace)
    pYell = all isUpper
    pSilence = null s || all isSpace s
