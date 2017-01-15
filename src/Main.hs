
module Main where

-- base
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newMVar)
-- async
import Control.Concurrent.Async (withAsync)
-- random
import System.Random
-- MonadRandom
import Control.Monad.Random.Strict
-- galaxy-break
import ConcurrentConsole (consoleInit, atomicGetOneChar, atomicPrintLn)
import PCGen

-- counts off a number every 1 second.
counter :: MVar [Char] -> IO ()
counter lock = counter' 0 where
    counter' n = do
        threadDelay 1000000 -- 1 second
        atomicPrintLn lock $ "counter> " ++(show n)
        counter' (n+1)

-- computes the sum of x random values
randSum :: Int -> MVar [Char] -> IO ()
randSum count lock = do
    atomicPrintLn lock $ "randSum> Beginning a sum of "++(show count)++" randoms..."
    startGen <- randomIO :: IO PCGen
    let (results, resultGen) = runRand (replicateM count (getRandom :: MonadRandom m => m Word)) startGen
    atomicPrintLn lock $ "randSum> Total: " ++ show (sum results)
    atomicPrintLn lock $ "randSum> Final Generator: " ++ show resultGen

main :: IO ()
main = do
    consoleInit
    theLock <- newMVar [] -- the console's buffer starts empty
    atomicPrintLn theLock "main> I will count, you can also say things for me to say."
    atomicPrintLn theLock "main> (An empty line will quit the program.)"
    let loop = do
            result <- atomicGetOneChar theLock
            case result of
                Nothing -> loop
                Just line -> do
                    atomicPrintLn theLock $ "main> Input:" ++ line
                    if null line
                        then atomicPrintLn theLock $ "main> Exiting."
                        else loop
    withAsync (counter theLock) $ \_ ->
        withAsync (randSum 10000000 theLock) $ \_ ->
            loop
-- fin
