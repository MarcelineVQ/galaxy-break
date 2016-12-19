
module Main where

-- base
import System.IO
-- galaxy-break
import Control.Console

main :: IO ()
main = do
    consoleInit
    putStrLn "Type some lines. Blank line to quit."
    let loop = do
            putStr ">"
            line <- consoleGetLine
            if null line || head line == '\EOT'
                then putStrLn "Exiting..."
                else putStrLn ("you typed:" ++ line) >> loop
    loop
-- fin
