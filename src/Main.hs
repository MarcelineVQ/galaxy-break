{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Main where

import qualified Language.C.Inline as C
import Foreign.C.Types
import Foreign.Ptr
import System.IO
import System.Win32.Types (DWORD)

C.context C.baseCtx
C.include "windows.h"

{-| Does the complete setup and tear down to read a single, unbuffered keypress
in the windows console.
-}
completeReadChar :: IO CChar
completeReadChar = [C.block| char {
    // SetConsoleMode
    // https://msdn.microsoft.com/en-us/library/ms686033(VS.85).aspx
    // INPUT_RECORD struct
    // https://msdn.microsoft.com/en-us/library/windows/desktop/ms683499(v=vs.85).aspx
    // KEY_EVENT_RECORD struct
    // https://msdn.microsoft.com/en-us/library/windows/desktop/ms684166(v=vs.85).aspx
    // Virtual-Key Codes
    // https://msdn.microsoft.com/en-us/library/windows/desktop/dd375731(v=vs.85).aspx
    // This code is based on a c++ help forum answer:
    // http://www.cplusplus.com/forum/beginner/3329/
    DWORD        mode;
    HANDLE       hstdin;
    INPUT_RECORD inrec;
    DWORD        count;
    char         result = '\0';
    
    hstdin = GetStdHandle( STD_INPUT_HANDLE );
    if (hstdin == INVALID_HANDLE_VALUE || !GetConsoleMode(hstdin, &mode) || !SetConsoleMode(hstdin, 0))
        return result;
    
    //FlushConsoleInputBuffer( hstdin );

    // Wait for a single key press
    do
        ReadConsoleInput( hstdin, &inrec, 1, &count );
    while ((inrec.EventType != KEY_EVENT) || !inrec.Event.KeyEvent.bKeyDown);

    // Remember which key the user pressed
    result = inrec.Event.KeyEvent.uChar.AsciiChar;

    // This is how we could wait for a key to be released.
    // However, doing this makes the input very slow and unresponsive.
    // the user hasn't always released enough of the last key to fire this event
    // before they've begun to type the next key.
    //do
    //    ReadConsoleInput( hstdin, &inrec, 1, &count );
    //while ((inrec.EventType != KEY_EVENT) || inrec.Event.KeyEvent.bKeyDown);

    // Restore the original console mode before leaving this func.
    SetConsoleMode( hstdin, mode );

    return result;
    } |]

{-| Sets the console mode to be immediate input instead of line input,
which implicitly also disables echoing. Returns if it was successful or not.
-}
setImmediateMode :: IO Bool
setImmediateMode = (/= 0) <$> [C.block| unsigned int {
    HANDLE hstdin = GetStdHandle(STD_INPUT_HANDLE);
    if (hstdin != INVALID_HANDLE_VALUE)
        return SetConsoleMode(hstdin, 0);
    else
        return 0;
    } |]

{-| Reads console input events until there is an event for an ASCII character
and then returns the character involved. We have to skip over some events, because
something like the key combination "shift-a" will have an input event for just the
shift key and then a seperate input event for the 'A' character.

If the standard input handle can't be obtained then this will return '\0' instead.
-}
onlyReadChar :: IO Char
onlyReadChar = (toEnum . fromEnum) <$> [C.block| char {
    char result;
    HANDLE hstdin = GetStdHandle(STD_INPUT_HANDLE);
    if (hstdin == INVALID_HANDLE_VALUE) {
        result = '\0';
    } else {
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

lineGrab :: String -> IO String
lineGrab lookback = do
    c <- onlyReadChar -- on unix we'd use getChar in unbuffered mode.
    case c of
        '\r' -> putStr "R\r\n" >> hFlush stdout >> return (reverse lookback) -- windows uses \r\n at the end of lines.
        '\n' -> putStr "N\n" >> hFlush stdout >> return (reverse lookback)
        '\b' -> putStr "\b \b" >> hFlush stdout >> lineGrab (drop 1 lookback)
        '\t' -> putStr "1234"  >> lineGrab ("4321"++lookback) -- tab data is put into the lookback reversed
        _ -> putChar c >> hFlush stdout >> lineGrab (c:lookback)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    --hSetBuffering stdin NoBuffering -- this is what we'd use on unix.
    setImmediateMode
    putStr "TypeALine>"
    line <- lineGrab ""
    putStrLn $ "Line was:" ++ line
