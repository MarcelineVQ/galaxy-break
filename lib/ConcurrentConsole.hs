{-# LANGUAGE Trustworthy, CPP, Trustworthy, GeneralizedNewtypeDeriving,
MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}

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
#if defined Win32flag
import ConcurrentConsole.Win32 (consoleInit, readOneChar)
#elif defined Linuxflag
import ConcurrentConsole.Linux (consoleInit, readOneChar)
#else
-- support for other systems should go here, hashtag-yolo
import YourSystemDoesntHaveConConsoleSupport
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

{-| The ConConsole wraps immediately over IO and lets you use the console
concurrently. That is, if the user has a partly entered command, the print
operations will delete the buffered input, print their output, and then
restore the buffered input as it was. This allows background threads to safely
print things out without the user's input getting all spread out. There's
still only source of input of course, because the user has only one input
buffer. When you're using this you'll probably want to use something like the
`lifted-async` package rather than using forkIO or trying to lift the `async`
package yourself.
-}
newtype ConConsole a = ConConsole (ReaderT (MVar [Char]) IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadBase IO)

{-| This is magic that I don't fully understand, all the thanks go to cocreature
of #haskell. This lets you use ConConsole with the `lifted-async` functions.
-}
instance MonadBaseControl IO ConConsole where
    type StM ConConsole a = StM (ReaderT (MVar [Char]) IO) a
    liftBaseWith f = ConConsole (liftBaseWith (\f' -> f (\(ConConsole r) -> f' r)))
    restoreM st = ConConsole (restoreM st)

{-| Performs the ConConsole action within IO.
-}
runConConsole :: ConConsole a -> IO a
runConConsole (ConConsole r) = do
    consoleInit
    lock <- newMVar []
    runReaderT r lock

{-| Prints the string given, respecting the current input. Before the printing
process begins, the string is fully evaluated, which ensures that any work
required to evaluate the string is already done by the time that the lock on
the console is taken, allowing others to print during that time.
-}
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

{-| It is intended that the action for the main thread "ends" with this action
after any necessary setup. This is a specialized sort of loop: it reads one
character at a time (blocking if necessary), then grabs the conconsole lock,
echoes the input to the screen as necessary, and adds the input to the
buffer. If the character is one of a few special characters, a bit of
additional processing is performed (proper support for backspace, tab key
inserts 4 spaces, \r, \n, and Ctrl+D end the line). If there's an end of line
then the buffer is passed along to the callback given, which should determine
if the loop should run agian. For the UI to remain responsive, the callback
should return as fast as possible and divert any actual work into side
threads. Particularly, the system will keep building up key input events in
the queue even if this action is blocked in the callback or by a GC pause or
something else, but the user just won't see the keys appearing on screen.

In the future, it is intended that the key processing will be incorporated
into how the callback works so that the loop is more customizable.
-}
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
            '\b' -> if null buffer
                -- don't backspace earlier than the "start" of the input point.
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
