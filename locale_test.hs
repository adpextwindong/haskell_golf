import GHC.IO.Encoding (getLocaleEncoding, setLocaleEncoding, utf8)

--https://stackoverflow.com/a/33157213

main :: IO ()
main = do
    setLocaleEncoding utf8
    getLocaleEncoding >>= print
