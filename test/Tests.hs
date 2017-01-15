
module Main where

import Test.Hspec

import PCGenTests

main :: IO ()
main = hspec $ do
    pcGenTests
