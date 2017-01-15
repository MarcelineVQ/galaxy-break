{-# LANGUAGE CPP, Trustworthy, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}

module ConcurrentConsole (
    ConConsole(),
    runConConsole,
    conPutStrLn,
    conConsoleMain
    ) where

{- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
Here begins the system-dependent imports for the primitive operations that the
other operations within this module will depend upon.
-} -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
#ifdef Win32flag
import ConcurrentConsole.Win32 (consoleInit, readOneChar)
#else
support for other systems should go here, hashtag-yolo
#endif
{- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
consoleInit and readOneChar must have been somehow imported by now or the rest
of the module, which depends on those two functions, will not compile at all.
-} -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- base
import System.IO
import Control.Concurrent.MVar
import Control.Monad (unless)
import Control.Exception (evaluate)
import Control.Exception
-- deepseq
import Control.DeepSeq
-- mtl
import Control.Monad.Reader
import Control.Monad.Reader.Class
-- transformers-base
import Control.Monad.Base
-- monad-control
import Control.Monad.Trans.Control

{-| The ConConsole lets you use the console concurrently in a proper way.
-}
newtype ConConsole a = ConConsole (ReaderT (MVar [Char]) IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadBase IO)

{-| This is magic that I don't fully understand, all the thanks go to
cocreature of #haskell.
-}
instance MonadBaseControl IO ConConsole where
  type StM ConConsole a = StM (ReaderT (MVar [Char]) IO) a
  liftBaseWith f = ConConsole (liftBaseWith (\f' -> f (\(ConConsole r) -> f' r)))
  restoreM st = ConConsole (restoreM st)

runConConsole :: ConConsole a -> IO a
runConConsole (ConConsole r) = do
    consoleInit
    lock <- newMVar []
    runReaderT r lock

conPutStrLn :: String -> ConConsole ()
conPutStrLn message = ConConsole $ do
    lock <- ask
    evaluatedMessage <- liftIO $ evaluate $ force $ message
    liftIO $ withMVar lock $ \buffer -> do
        -- clear the currently displayed buffer text,
        -- this goofy way of doing it is thanks to dmwit of #haskell.
        putStr $ "\b \b" <* buffer
        -- print the message
        putStrLn evaluatedMessage
        -- restore the displayed buffer
        putStr (reverse buffer)
        hFlush stdout

conConsoleMain :: (String -> ConConsole Bool) -> ConConsole ()
conConsoleMain action = do
    c <- liftIO readOneChar
    lock <- ConConsole ask
    -- in this situation,
    -- modifyMVar :: MVar [Char] -> ([Char] -> IO ([Char], Maybe String)) -> IO (Maybe String)
    maybeLine <- liftIO $ modifyMVar lock $ \buffer -> do
        case c of
            '\r' -> do
                -- windows uses \r at the end of lines.
                putStr "\n"
                hFlush stdout
                return ([], Just $ reverse buffer)
            '\n' -> do
                -- unix uses \n.
                putStr "\n"
                hFlush stdout
                return ([], Just $ reverse buffer)
            '\EOT' -> do
                -- we will treat C-d as if it was Enter.
                putStr "\n"
                hFlush stdout
                return ([], Just $ reverse buffer)
            '\b' -> if null buffer -- don't backspace earlier than the "start" of the input point.
                then return ([], Nothing)
                else do
                    putStr "\b \b"
                    hFlush stdout
                    return (drop 1 buffer, Nothing)
            '\t' -> do
                putStr "    "
                hFlush stdout
                return ("    "++buffer,Nothing)
            _ -> do
                putChar c
                hFlush stdout
                return (c:buffer,Nothing)
    case maybeLine of
        Nothing -> conConsoleMain action
        Just line -> do
            goAgain <- action line
            if goAgain
                then conConsoleMain action
                else return ()
