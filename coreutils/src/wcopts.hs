module Main where

import Options.Applicative
import Data.Semigroup ((<>))

data WcApp = WcApp { appBytes :: Bool
                   , appChars :: Bool
                   , appLines :: Bool
                   , appMaxLineLength :: Bool
                   , appWords :: Bool
                   , appHelp :: Bool
                   , appVersion :: Bool
                   }

argpBytes :: Parser Bool
argpBytes = switch ( short 'c' <> long "bytes" <> help "print the byte counts" )
argpChars = switch ( short 'm' <> long "chars" <> help "print the character counts" )
argpLines = switch ( short 'l' <> long "lines" <> help "print the newline counts" )
argpMaxLineLength = switch ( short 'L' <> long "max-line-length" <> help "print the maximum display width" )
argpWords = switch ( short 'w' <> long "words" <> help "print the word counts" )
argpHelp = switch ( long "help" <> help "display this help and exit" )
argpVersion = switch ( long "help" <> help "output version information and exit" )

main :: IO ()
main = return ()