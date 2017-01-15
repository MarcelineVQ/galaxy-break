module ConcurrentConsole.Linux (
    consoleInit,
    readOneChar
    ) where

-- base
import System.IO

-- | Make the console ready for use in a platform dependent manner.
consoleInit :: IO Bool
consoleInit = do
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
    return True

readOneChar :: IO Char
readOneChar = getChar
