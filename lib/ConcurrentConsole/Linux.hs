{-# LANGUAGE Safe #-}

module ConcurrentConsole.Linux (
    consoleInit,
    readOneChar
    ) where

-- base
import System.IO

-- | Make the console ready for use in a platform dependent manner.
consoleInit :: IO Bool
consoleInit = do
    hSetBuffering stdout (BlockBuffering Nothing)
    hSetEcho stdin False
    return True

readOneChar :: IO Char
readOneChar = getChar
