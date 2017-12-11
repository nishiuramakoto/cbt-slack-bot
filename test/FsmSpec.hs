{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module FsmSpec where

import Fsm

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.Tasty.Golden

import Data.List
import Data.Ord

import qualified Data.List as List
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Control.Monad.State as SM
import qualified FRP.Yampa as Yampa

import Control.Arrow
import Control.Monad
import Text.Show.Unicode
import System.IO
import System.IO.Error

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

diff ref new = ["diff"
               , "-u" -- 3 lines of context
               , "--ignore-space-change"
               , ref, new]

type YampaTestMonad a = SM.State (Int, [String]) a
putTestLn :: String -> YampaTestMonad ()
putTestLn s = SM.modify (\(x,ss) -> (x, (s ++ "\n"):ss))

getN :: YampaTestMonad Int
getN = SM.get >>= return . fst

incN :: YampaTestMonad ()
incN = SM.modify (\(x,ss) -> (x+1, ss))

unitTests = testGroup "Unit tests"
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT

  , goldenVsStringDiff "golden test" diff "test/hello.golden" $ do
    return "hello world!\n"

  , goldenVsStringDiff "hello yampa" diff "test/hello-yampa.golden" $ do
    let init :: YampaTestMonad Int
        init = do
          putTestLn "init"
          return 0

        sensor :: Bool -> YampaTestMonad (Yampa.DTime, Maybe Int)
        sensor can_block = do
          let dt = 1
          incN
          x <- getN
          return (dt, Just x)

        actuator :: Bool -> Int -> YampaTestMonad Bool
        actuator changed out = do
          let stop = out >= 5
          putTestLn $ "test" ++ (ushow out)
          return stop

        sf :: Yampa.SF Int Int
        sf = Yampa.identity

        (out, ss) = SM.execState
                    (Yampa.reactimate init sensor actuator sf)
                    (0,[])

    return $ BS.pack $ List.concat $ reverse ss

  -- , goldenVsFileDiff "yampa interactive" diff
  --   "test/io-yampa.golden"
  --   "test/io-yampa.out" $
  --   withFile "test/io-yampa.out" WriteMode $ \hOut ->
  --   withFile "test/io-yampa.in"  ReadMode  $ \hIn  -> do

  --     let puts = hPutStrLn hOut
  --         gets = hGetLine  hIn `catchIOError` \e -> return "<EOF>"

  --         init :: IO Input
  --         init = do
  --           puts "init"
  --           return "//start"

  --         sensor :: Bool -> IO (Yampa.DTime, Maybe Input)
  --         sensor can_block = do
  --           let dt = 1
  --           x <- gets
  --           return (dt, Just x)

  --         actuator :: Bool -> Output -> IO Bool
  --         actuator changed out = do
  --           let stop = out == "<EOF>"
  --           puts out
  --           return stop

  --         sf :: Yampa.SF Input Output
  --         sf = Yampa.identity

  --     Yampa.reactimate init sensor actuator sf

  , goldenVsFileDiff "yampa stm" diff
    "test/stm-yampa.golden"
    "test/stm-yampa.out" $
    withFile "test/stm-yampa.out" WriteMode $ \hOut ->
    withFile "test/stm-yampa.in"  ReadMode  $ \hIn  -> do

      let puts :: String -> IO ()
          puts s = hPutStrLn hOut s

          gets :: IO InputEof
          gets = (Just <$> hGetLine hIn)
                 `catchIOError`
                 \e -> return Nothing

          init :: IO BotInput
          init = do
            dump "<init>"
            return (Just ":start")

          sensor :: Bool
                 -> IO (Yampa.DTime, Maybe BotInput)
          sensor can_block = do
            let dt = 1
            x <- gets
            dump ("in:" ++ ushow x)
            return (dt, Just x)

          actuator :: Bool -> [BotOutput] -> IO Bool
          actuator changed outs = do
            stops <- mapM (actuate changed) outs
            let stop = any (== True) stops
            return stop

            where
              actuate :: Bool -> BotOutput -> IO Bool
              actuate changed out = do
                dumpS ("> " ++ ushow out)
                let stop = isFinalBotState out
                puts (ushow out)
                return stop

      Yampa.reactimate init sensor actuator cbt

  ]
