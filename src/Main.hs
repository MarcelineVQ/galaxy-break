{-# LANGUAGE TypeApplications, NondecreasingIndentation #-}

module Main where

-- base
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newMVar)
-- lifted-async
import Control.Concurrent.Async.Lifted (withAsync)
-- random
import System.Random
-- MonadRandom
import Control.Monad.Random.Strict
-- galaxy-break
import ConcurrentConsole
import PCGen

-- counts off a number every 1 second.
counter :: ConConsole ()
counter = counter' 0 where
    counter' n = do
        liftIO $ threadDelay 1000000 -- 1 second
        conPutStrLn $ "counter> " ++(show n)
        counter' (n+1)

-- computes the sum of x random values
randSum :: Int -> ConConsole ()
randSum count = do
    conPutStrLn $ "randSum> Beginning a sum of "++(show count)++" randoms..."
    startGen <- liftIO $ randomIO :: ConConsole PCGen
    let (results, resultGen) = runRand @PCGen @[Word] (take count <$> getRandoms) startGen
    conPutStrLn $ "randSum> Total: " ++ show (sum results)
    conPutStrLn $ "randSum> Final Generator: " ++ show resultGen

againAction :: String -> ConConsole Bool
againAction string = do
    if null string
        then do
            conPutStrLn "main> Empty line, exiting."
            return False
        else do
            conPutStrLn $ "main> Input:" ++ string
            return True

main :: IO ()
main = runConConsole $ do
    conPutStrLn "main> I will count, you can also say things for me to say."
    conPutStrLn "main> (An empty line will quit the program.)"
    withAsync counter $ \_ -> do
    withAsync (randSum 10000000) $ \_ -> do
    conConsoleMain againAction
-- fin
