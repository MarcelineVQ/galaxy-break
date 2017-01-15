{-# LANGUAGE CPP, MagicHash, BangPatterns, Trustworthy #-}

{-| This contains a random number generator based upon the C / C++ version of
the concept written by M.E. O'Neill of <pcg-random.org>. Her version is
Copyright 2014 to her, and is used here under the Apache 2.0 license. This
file (as well as galaxy-break in general) is under the AGPL license, and GNU's
website says that they're compatible licenses.
-}
module PCGen (
    PCGen,
    mkPCGen
    ) where

-- base
import Data.Bits
import Data.Word
import Data.Int
import Data.List
import Data.Char
import GHC.Exts
-- random
import System.Random

#ifdef SixtyFourBit
{-
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
--    __ _  _     ____  _ _      _____           _
--   / /| || |   |  _ \(_) |    / ____|         | |
--  / /_| || |_  | |_) |_| |_  | (___  _   _ ___| |_ ___ _ __ ___  ___
-- | '_ \__   _| |  _ <| | __|  \___ \| | | / __| __/ _ \ '_ ` _ \/ __|
-- | (_) | | |   | |_) | | |_   ____) | |_| \__ \ ||  __/ | | | | \__ \
--  \___/  |_|   |____/|_|\__| |_____/ \__, |___/\__\___|_| |_| |_|___/
--                                      __/ |
--                                     |___/
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
On 64-bit machines a Word# will have the same width as the Word64 that we want
to use, so we use Word# and primitive operations for a little bit of a speed
boost.
-}

-- -- -- -- --
-- PCGen32 Section (64-bit)
-- -- -- -- --

{-| A permuted congruential generator that produces 32 bits of randomness per step.
-}
data PCGen = PCGen Word# Word#

{-| Given two integral values, constructs a PCGen and runs the generator once,
giving you back the resulting generator. The generator is run once
automatically because most initial state values that a human picks end up
giving 0 as the first result, which is pretty un-random feeling overall.
-}
mkPCGen :: (Num b, Integral a) => a -> a -> PCGen
mkPCGen stateIn incIn = let
    !(W# st) = fromIntegral stateIn
    !(W# inc) = fromIntegral incIn
    in snd $ next $ PCGen st (or# inc 1##)

{-| The genRange of a PCGen is 0 through 4,294,967,295 (aka
0xFFFFFFFF, all lower 32 bits active).
-}
instance RandomGen PCGen where
    next !(PCGen st inc) = (outInt,outGen) where
        xorShifted = uncheckedShiftRL# (xor# (uncheckedShiftRL# st 18#) st) 27#
        rot = uncheckedShiftRL# st 59#
        w = or# (uncheckedShiftRL# xorShifted (word2Int# rot)) (uncheckedShiftL# xorShifted (word2Int# (and# (minusWord# 0## rot) 31##)))
        newState = plusWord# (timesWord# st 6364136223846793005##) inc
        outGen = PCGen newState inc
        outInt = I# (word2Int# (narrow32Word# w))
    genRange _ = (I# 0#,I# 0xFFFFFFFF#)
    split !gen@(PCGen st inc) = (outA, outB)
        where -- no statistical foundation for this!
            !((I# q),nGen1) = next gen
            !((I# w),nGen2) = next nGen1
            !((I# e),nGen3) = next nGen2
            !((I# r),(PCGen stFour incFour)) = next nGen3
            !(W# stateA) = (W# stFour) `rotateR` 5
            !(W# stateB) = (W# stFour) `rotateR` 3
            incA = or# (uncheckedShiftL# (int2Word# q) 32#) (int2Word# w)
            incB = or# (uncheckedShiftL# (int2Word# e) 32#) (int2Word# r)
            outA = PCGen stateA (or# incA 1##)
            outB = PCGen stateB (or# incB 1##)

{-| randomR is the same as random for this type.
-}
instance Random PCGen where
    random gen = let
        (x,newGen) = (random gen)
        in (mkPCGen (x::Word) x,newGen)
    randomR (_,_) gen = let
        (x,newGen) = random gen
        in (mkPCGen (x::Word) x,newGen)

{-| PCGen values are equal when they have equal state and incriment values.
-}
instance Eq PCGen where
    (==) !(PCGen stA incA) !(PCGen stB incB) = isTrue# (andI# (eqWord# stA stB) (eqWord# incA incB))

{-| PCGen are ordered by inc, then by state. The order isn't intended to
be used for anything other than to potentially put PCGen values into a
'Set'.
-}
instance Ord PCGen where
    compare !(PCGen stA incA) !(PCGen stB incB) = case compare (W# incA) (W# incB) of
        LT -> LT
        EQ -> compare (W# stA) (W# stB)
        GT -> GT

showReadPrefix :: String
showReadPrefix = "PCGen"

{-| The show and read for PCGen will remake the exact same PCGen.
-}
instance Show PCGen where
    show !(PCGen st inc) = showReadPrefix ++ " " ++ show (W# st) ++ " " ++ show (W# inc)

{-| The show and read for PCGen will remake the exact same PCGen. This
instance ensures that the inc value is always odd, but that won't affect you
if you only get PCGen strings from uses of the show function.
-}
instance Read PCGen where
    -- readsPrec :: Int -> String -> [(a, String)]
    readsPrec prec str = if (showReadPrefix ++ " ") `isPrefixOf` str
        then let strNoConstructor = drop (length (showReadPrefix ++ " ")) str in
            case readsPrec prec strNoConstructor of
                [(stateWord,statelessString)] -> case readsPrec prec (drop 1 statelessString) of
                    [(incWord,wordlessString)] -> let
                        !(W# st) = stateWord
                        !(W# inc) = incWord
                        in [(PCGen st (or# inc 1##),wordlessString)]
                    _ -> []
                _ -> []
        else []

#else
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
--  ____ ___    ____  _ _      _____           _
-- |___ \__ \  |  _ \(_) |    / ____|         | |
--   __) | ) | | |_) |_| |_  | (___  _   _ ___| |_ ___ _ __ ___  ___
--  |__ < / /  |  _ <| | __|  \___ \| | | / __| __/ _ \ '_ ` _ \/ __|
--  ___) / /_  | |_) | | |_   ____) | |_| \__ \ ||  __/ | | | | \__ \
-- |____/____| |____/|_|\__| |_____/ \__, |___/\__\___|_| |_| |_|___/
--                                    __/ |
--                                   |___/
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
{-
On 32-bit machines, I really don't want to deal with the troubles of making
two Word# values work as a single Word64, so we'll just let our data stay
wrapped up. Things are still pretty fast, just not as fast.
-}

-- -- -- -- --
-- PCGen32 Section (32-bit)
-- -- -- -- --

{-| A Permuted Congruential Generator that produces 32-bits of output per
step. Equal when both internal values are equal. Ordered in some arbitrary way
that doesn't matter, it's just so you can put it into a set. Read and Show are
both derived, allowing you to serialize your PCGen, but this derived Read won't
ensure that the inc value is odd if you give it a string that you made
yourself instead of a string that came from show.
-}
data PCGen = PCGen {
    _state32 :: {-# UNPACK #-} !Word64, -- ^ The internal state of the generator.
    _inc32 :: {-# UNPACK #-} !Word64 -- ^ controls what number stream the generator moves along.
    } deriving (Eq, Ord, Read, Show)

{-| The Inc value on a PCGen must always be odd, so this ensures that that is
always the case. It also runs the generator once to advance the seed to a more
useful value, otherwise you almost always get 0 as your first result, which
never feels very random.
-}
mkPCGen :: (Num b, Integral a) => a -> a -> PCGen
mkPCGen state inc = snd $ stepPCGen $ PCGen state (inc .|. 1)

-- | Given a PCGen, produces the next random 32 bits, and the next PCGen.
stepPCGen :: PCGen -> (Word, PCGen)
stepPCGen (PCGen32 state inc) = (w, newGen)
    where xorshifted = ((state `shiftR` 18) `xor` state) `shiftR` 27
          rot = fromIntegral $ state `shiftR` 59
          w = fromIntegral $ (xorshifted `shiftR` rot) .|. (xorshifted `shiftL` ((-rot) .&. 31))
          newState = state * 6364136223846793005 + inc
          newGen = PCGen32 newState inc

{-| Mostly what you'd expect. Note that the 32 bits of output are forced into
being returned as an Int32 by the interface, so the `genRange` goes both above
and below zero. (In the 64-bit version it uses the range of a Word32 directly,
since that fits entirely within an Int64.)
-}
instance RandomGen PCGen where
    next gen = (outInt, nextGen)
        where (outWord, nextGen) = stepPCGen gen
              outInt = fromIntegral (fromIntegral outWord :: Int32)
    genRange _ = (fromIntegral (minBound :: Int32),fromIntegral (maxBound :: Int32))
    split gen@(PCGen state inc) = (outA, outB)
        where -- no statistical foundation for this!
            (q,nGen1) = stepPCGen gen
            (w,nGen2) = stepPCGen nGen1
            (e,nGen3) = stepPCGen nGen2
            (r,nGen4) = stepPCGen nGen3
            stateA = (_state nGen4) `rotateR` 5
            stateB = (_state nGen4) `rotateR` 3
            incA = ((fromIntegral q) `shiftL` 32) .|. (fromIntegral w)
            incB = ((fromIntegral e) `shiftL` 32) .|. (fromIntegral r)
            outA = mkPCGen32 stateA incA
            outB = mkPCGen32 stateB incB

{-| randomR is the same as random for this type.
-}
instance Random PCGen where
    random gen = let
        (x,newGen) = random gen
        in (mkPCGen x x,newGen)
    randomR (_,_) gen = let
        (x,newGen) = random gen
        in (mkPCGen x x,newGen)

#endif
