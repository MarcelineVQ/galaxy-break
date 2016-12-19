{-# LANGUAGE CPP, Trustworthy #-}

module Control.Console (
    consoleInit,
    consoleGetLine
    ) where

#ifdef Win32

import Control.Console.Win32 (consoleInit, consoleGetLine)

#else

support for other systems should go here

#endif