splitBy :: String -> Char -> [String]
splitBy [] d = []
splitBy s@(x:xs) d = chunk : (splitBy rest d)
  where
    start = if x == ' ' then xs else s
    chunk = takeWhile (\c -> c /= d) start
    rest = drop (length chunk) start
