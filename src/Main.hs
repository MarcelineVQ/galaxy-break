{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Main where

import qualified Language.C.Inline as C
import Foreign.C.Types
import Foreign.Ptr

C.context C.baseCtx
C.include "stdio.h"
C.include "windows.h"

main :: IO ()
main = do
    cChar <- [C.block| char {
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
        char         default_prompt[] = "Press the 'any' key...";
        char         result           = '\0';

        /* Set the console mode to no-echo, raw input, */
        /* and no window or mouse events.              */
        hstdin = GetStdHandle( STD_INPUT_HANDLE );
        if (hstdin == INVALID_HANDLE_VALUE
            || !GetConsoleMode( hstdin, &mode )
            || !SetConsoleMode( hstdin, 0 ))
            return result;

        /* Instruct the user */
        WriteConsole(
            GetStdHandle( STD_OUTPUT_HANDLE ),
            default_prompt,
            lstrlen( default_prompt ),
            &count,
            NULL
            );

        FlushConsoleInputBuffer( hstdin );

        /* Wait for and get a single key PRESS */
        do
            ReadConsoleInput( hstdin, &inrec, 1, &count );
        while ((inrec.EventType != KEY_EVENT) || !inrec.Event.KeyEvent.bKeyDown);

        /* Remember which key the user pressed */
        result = inrec.Event.KeyEvent.uChar.AsciiChar;

        /* Wait for and get a single key RELEASE */
        do
            ReadConsoleInput( hstdin, &inrec, 1, &count );
        while ((inrec.EventType != KEY_EVENT) || inrec.Event.KeyEvent.bKeyDown);

        /* Restore the original console mode */
        SetConsoleMode( hstdin, mode );

        return result;
        } |]
    let c = toEnum $ fromEnum cChar :: Char
    if c /= '\0'
        then putStr "\nKey Was:" >> putStrLn [c]
        else putStrLn "Could not read."
