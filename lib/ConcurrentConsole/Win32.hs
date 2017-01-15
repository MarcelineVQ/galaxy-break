{-# LANGUAGE QuasiQuotes, TemplateHaskell, Trustworthy #-}

module ConcurrentConsole.Win32 (
    consoleInit,
    readOneChar
    ) where

-- base
import System.IO
import Foreign.Ptr
import Foreign.C.Types
-- inline-c
import qualified Language.C.Inline as C

-- These must be at the top of the file for TemplateMagic to happen.
C.context C.baseCtx
C.include "windows.h"

-- | Make the console ready for use in a platform dependent manner.
consoleInit :: IO Bool
consoleInit = do
    hSetBuffering stdout NoBuffering
    setImmediateMode

{-| Sets the console mode to give immediate input instead of line input, which
implicitly also disables echoing. Returns if it was successful or not.
-}
setImmediateMode :: IO Bool
setImmediateMode = (/= 0) <$> [C.block| unsigned int {
    // https://msdn.microsoft.com/en-us/library/ms683231(v=vs.85).aspx
    HANDLE hstdin = GetStdHandle(STD_INPUT_HANDLE);
    if (hstdin != INVALID_HANDLE_VALUE)
        // https://msdn.microsoft.com/en-us/library/ms686033(VS.85).aspx
        return SetConsoleMode(hstdin, 0);
    else
        return 0;
    } |]

{-| Reads console input events until there is an event for an ASCII character
and then returns the character involved. We have to skip over some events,
because something like pressing and holding shift and then pressing 'a' is two
events: one for the shift key press and then a seperate input event for the
'a' key press, producing the 'A' character in the end. Similarly, Alt and Ctrl
both produce a key press event without producing a usable result. This call
blocks until there eventually is an appropriate key press.

If the standard input handle can't be obtained then this will return '\0'
instead.
-}
readOneChar :: IO Char
readOneChar = (toEnum . fromEnum) <$> [C.block| char {
    char result;
    // https://msdn.microsoft.com/en-us/library/ms683231(v=vs.85).aspx
    HANDLE hstdin = GetStdHandle(STD_INPUT_HANDLE);
    if (hstdin == INVALID_HANDLE_VALUE) {
        result = '\0';
    } else {
        // https://msdn.microsoft.com/en-us/library/ms683499(v=vs.85).aspx
        INPUT_RECORD inrec;
        DWORD numberOfEventsRead;
        do {
            // https://msdn.microsoft.com/en-us/library/ms684961(v=vs.85).aspx
            ReadConsoleInput(hstdin, &inrec, 1, &numberOfEventsRead);
        } while ((inrec.EventType != KEY_EVENT) ||
            !inrec.Event.KeyEvent.bKeyDown ||
            !inrec.Event.KeyEvent.uChar.AsciiChar);
        result = inrec.Event.KeyEvent.uChar.AsciiChar;
    }
    return result;
    } |]
