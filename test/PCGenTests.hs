{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-| This tests the Data.PCGen module. Because Data.PCGen uses conditional
compilation to have different versions if you're on 32-bit or 64-bit, then this
module must also run slightly different tests on 32 and 64 bit versions. The
actual properties tested are the same either way though.
-}
module PCGenTests (
    pcGenTests
    ) where

import Test.Hspec
import Test.QuickCheck

import System.Random

import PCGen

-- -- -- -- --
-- Instances for Arbitrary PCGen values.
-- -- -- -- --

-- | Orphan, but that's alright.
instance Arbitrary PCGen where
    arbitrary = do
        w1 <- arbitrary
        w2 <- arbitrary
        return (mkPCGen (w1 :: Word) w2)

propGenRange :: (RandomGen g) => g -> g -> Bool
propGenRange = \rgenA rgenB -> genRange rgenA == genRange rgenB

propNextInRange :: (RandomGen g) => g -> Bool
propNextInRange = \randomGen -> let
        (i,g) = next randomGen
        (low, high) = genRange g
        in i >= low && i <= high

propSplit :: (RandomGen g, Eq g) => g -> Bool
propSplit = \randomGen -> let
    (genL, genR) = split randomGen
    in (genL /= randomGen) && (genR /= randomGen) && (genL /= genR)

-- -- -- -- --
-- PCGen tests
-- -- -- -- --

pcGenTests :: Spec
#ifdef SixtyFourBit
pcGenTests = describe "PCGen (64-bit)" $ do
#else
pcGenTests = describe "PCGen (32-bit)" $ do
#endif

    it "mkPCGen always gives odd inc value." $ property $
#ifdef SixtyFourBit
        \gen -> odd (read (words (show (gen::PCGen)) !! 2) :: Word)
#else
        \gen -> odd ((read [last.init $ (show (gen::PCGen))])::Int)
#endif

    it "read can always reads back exactly what show generated" $ property $
        \gen -> gen == (read (show (gen::PCGen)))
        
    it "read can parse arbitrary semi-valid strings" $ property $
#ifdef SixtyFourBit
        \w1 w2 -> Prelude.seq (read ("PCGen "++show (w1::Word)++" "++show (w2::Word))::PCGen) True
#else
        \w1 w2 -> Prelude.seq (read ("PCGen {_state = "++show (w1::Word)++", _inc = "++show (w2::Word)++"}")::PCGen) True
#endif

    it "genRange ignores the input generator given" $ property $
        \genA genB -> propGenRange (genA::PCGen) (genB::PCGen)
        
    it "next is always within the bounds given by genRange" $ property $
        \gen -> propNextInRange (gen::PCGen)
        
    it "split results are distinct from both the input and each other" $ property $
        \gen -> propSplit (gen::PCGen)
