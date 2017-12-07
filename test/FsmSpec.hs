{-# LANGUAGE OverloadedStrings #-}
module FsmSpec where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.Tasty.Golden

import Data.List
import Data.Ord

import qualified FRP.Yampa as Yampa

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps = testGroup "(checked by SmallCheck)"
  [ SC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , SC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  ]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , QC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  ]

diff ref new = ["diff", "-u", ref, new]

unitTests = testGroup "Unit tests"
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT

  , goldenVsStringDiff "golden test" diff "test/hello.golden" $ do
    return "hello world!\n"

  , goldenVsStringDiff "hello yampa" diff "test/hello-yampa.golden" $ do
    let init :: IO Int
        init = do
          putStrLn "init"
          return 0

        sensor :: Bool -> IO (Yampa.DTime, Maybe Int)
        sensor can_block = do
          let x  = 0
              dt = 1
          putStrLn (show x)
          return (dt, Just x)

        actuator :: Bool -> Int -> IO Bool
        actuator changed out = do
          let stop = False
          putStrLn (show out)
          return stop

        sf :: Yampa.SF Int Int
        sf = pure id

    Yampa.reactimate init sensor actuator sf

  ]
