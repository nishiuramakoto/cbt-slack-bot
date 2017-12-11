{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module FsmSpec where

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
          putTestLn $ "test" ++ (show out)
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
            dump ("in:" ++ show x)
            return (dt, Just x)

          actuator :: Bool -> [BotOutput] -> IO Bool
          actuator changed outs = do
            stops <- mapM (actuate changed) outs
            let stop = any (== True) stops
            return stop

            where
              actuate :: Bool -> BotOutput -> IO Bool
              actuate changed out = do
                dumpS ("> " ++ show out)
                let stop = isFinalBotState out
                puts (show out)
                return stop

      Yampa.reactimate init sensor actuator cbt

  ]

dump :: Show a => a -> IO ()
dump x = putStrLn (show x)

dumpS :: String -> IO ()
dumpS x = putStrLn x

type Input  = String
type Output = String

type InputEof = Maybe Input

eof = Nothing

type Bot      = Yampa.SF BotInput [BotOutput]

type BotInput  = Maybe Input

data BotOutput
  = BotStart
  | BotAbort
  | BotFinish
  | BotMessage String
  | BotWarn    String
  | BotStackPush    CbtMethod CbtStack String
  | BotStackFreeze  CbtMethod CbtStack
  | BotMethodStart  CbtMethod
  | BotMethodAbort  CbtMethod
  | BotMethodFinish CbtMethod
  | BotMethodColumnStart  CbtMethod CbtStack
  | BotMethodColumnAbort  CbtMethod
  | BotMethodColumnFinish CbtMethod
  deriving (Eq, Ord, Show)

data CbtStack = CbtStackAT
              | CbtStackST
              | CbtStackEM
              | CbtStackCD
              | CbtStackRR
              deriving (Eq, Ord, Show)

data CbtMethod = CbtTripleColumnTechnique
               | CbtFiveColumnTechnique
               deriving (Eq, Ord, Show)

type CbtSessionEvent = Yampa.Event CbtMethod

cbtEvent :: CbtMethod -> CbtSessionEvent
cbtEvent method = Yampa.Event method

data CbtCommand = CbtSessionStart
                | CbtSessionAbort
                | CbtSessionFinish
                | CbtMethodStart   CbtMethod
                | CbtMethodAbort   CbtMethod
                | CbtMethodFinish  CbtMethod
                | CbtUnknownCommand String
                deriving (Eq, Ord, Show)

isFinalBotState :: BotOutput -> Bool
isFinalBotState BotFinish = True
isFinalBotState _         = False

zipWithA :: Arrow arrow
            => (a -> b -> c)
            -> arrow i a
            -> arrow i b
            -> arrow i c
zipWithA f x y = (x &&& y) >>^ uncurry f

type EvBot e = Yampa.SF BotInput ([BotOutput], e)

tag :: String -> Bot -> Bot
tag s = mapA (BotMessage s:)

tag' :: String -> EvBot e -> EvBot e
tag' s bot = zipWithA (\a (c,d) -> (a++c, d))
             (Yampa.constant [BotMessage s])
             bot

event = Yampa.Event
noEvent = Yampa.NoEvent

constA = Yampa.constant

mapA f sf = sf >>^ f

echo :: Bot
echo = Yampa.arr f
  where
    f (Just s) = [BotMessage s]
    f Nothing  = [BotFinish]

-- | Meant to be associative
dStep :: Yampa.SF a (b, Yampa.Event ())
      -> Yampa.SF a (b, Yampa.Event ())
      -> Yampa.SF a (b, Yampa.Event ())
dStep x y = x' `Yampa.dSwitch` const y
  where
    x' = proc a -> do
      (b, e) <- x -< a
      returnA -< ((b, noEvent), e)

dStepFold :: [ Yampa.SF a (b, Yampa.Event ()) ]
          -> Yampa.SF a (b, Yampa.Event ())
dStepFold = foldl1 dStep

cbt :: Yampa.SF BotInput [BotOutput]
cbt = cbtSession `Yampa.dSwitch` doMethod
  where
    doMethod method = case method of
      CbtTripleColumnTechnique ->
        dStepFold [ cbtColumn method CbtStackAT
                  , cbtColumn method CbtStackCD
                  , cbtColumn method CbtStackRR >>^ methodFinish method
                  ]
        `Yampa.dSwitch`
        const cbt

      CbtFiveColumnTechnique ->
        dStepFold [ cbtColumn method CbtStackST
                  , cbtColumn method CbtStackAT
                  , cbtColumn method CbtStackEM
                  , cbtColumn method CbtStackCD
                  , cbtColumn method CbtStackRR >>^ methodFinish method
                  ]
        `Yampa.dSwitch`
        const cbt

    methodFinish method (os, ev)
      | isEvent ev = (os ++ [BotMethodFinish method], ev)
      | otherwise  = (os, ev)

cbtSession :: Yampa.SF BotInput ([BotOutput], CbtSessionEvent)
cbtSession = proc input -> do
  case input of
    Just ":start" -> returnA -< ([BotStart], noEvent)
    Just ":help"  -> cbtSessionHelp -< ()
    Just ":3" -> returnA -< ([BotMethodStart CbtTripleColumnTechnique],
                             cbtEvent CbtTripleColumnTechnique)
    Just ":5" -> returnA -< ([BotMethodStart CbtFiveColumnTechnique],
                             cbtEvent CbtFiveColumnTechnique)
    Just ""   -> returnA -< ([BotWarn "empty line"], noEvent)
    Just xs   -> returnA -< ([BotWarn $ "unknown command:" ++ xs], noEvent)
    Nothing   -> returnA -< ([BotFinish], noEvent)

cbtSessionHelp :: Yampa.SF () ([BotOutput], CbtSessionEvent)
cbtSessionHelp = Yampa.constant
                 ([ BotMessage "help1"
                  , BotMessage "help2"
                  ] ,
                  noEvent)

-- Just warn once
cbtWarn :: String -> Yampa.SF BotInput [BotOutput]
cbtWarn prefix = Yampa.arr warn
  where
    warn input = case input of
        Just x  -> [BotWarn $ prefix ++ x ]
        Nothing -> [BotWarn $ prefix ++ "EOF" ]

putA :: String -> Yampa.SF a [BotOutput]
putA string = Yampa.constant [BotMessage string]

cbtPushLine :: CbtMethod
            -> CbtStack
            -> Bot
cbtPushLine method stack = Yampa.arr push
  where
    push input = case input of
      Just "" -> [BotStackFreeze method stack  ]
      Just l  -> [BotStackPush   method stack l]
      Nothing -> [BotStackFreeze method stack  ]

type UnitEvent = Yampa.Event ()

cbtBlankLineEvent :: Yampa.SF BotInput UnitEvent
cbtBlankLineEvent = Yampa.arr ev
  where
    ev input = case input of
      Just "" -> event ()
      Just l  -> noEvent
      Nothing -> event ()

cbtColumn :: CbtMethod
          -> CbtStack
          -> Yampa.SF BotInput ([BotOutput], Yampa.Event () )
cbtColumn method stack = cbtPushLine method stack
                         &&&
                         cbtBlankLineEvent


isEvent (Yampa.Event _) = True
isEvent (Yampa.NoEvent) = False
