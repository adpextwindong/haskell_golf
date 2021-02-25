module CamelCase where
import Data.Char

--Checks if the string contains delimiters
delimited :: Char -> String -> Bool
delimited d = foldr (||) False . map (== d)

data Delimiter = Underscore | Hyphen | None
    deriving (Show)

dChar :: Delimiter -> Char
dChar Underscore = '_'
dChar Hyphen = '-'
dChar None = '\0'

ourDelimiters :: String -> Delimiter
ourDelimiters s = if delimited '_' s
                  then Underscore
                  else
                    if delimited '-' s
                    then Hyphen
                    else
                      None

-- Splits a string by the given delimiters and drops the delimiters
splitByDelim :: Delimiter -> String -> [String]
splitByDelim d [] = []
splitByDelim d str = do
    let delimiter = dChar d
    let firstChunk = takeWhile (/= delimiter) str
    let tailChunk = dropWhile (/= delimiter) str

    if tailChunk /= []
    then firstChunk : splitByDelim d (tail tailChunk)
    else [firstChunk]
        
-- uppers the first letter of the word
upperFirsts :: String -> String
upperFirsts (x:xs) = (toUpper x) : xs
upperFirsts [] = []

-- Takes a single word that might be delimited, splits and uppers the first letters in the tail of the words
camel :: String -> String
camel w = case d of
  None -> w
  _ -> do 
    let (x:xs) = splitByDelim d w
    let upperized = x: map upperFirsts xs
    concat upperized
  where d = ourDelimiters w

toCamelCase :: String -> String
toCamelCase [] = []
toCamelCase str = do
    let camelized = unwords $ map camel $ words str
    if pascal
    then
        upperFirsts camelized
    else
        camelized
  where pascal = isUpper (str !! 0)