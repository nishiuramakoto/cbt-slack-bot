-- | Implementation of time-varied automata using FRP/Yampa
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows              #-}

module Fsm where

import Data.List
import Data.Ord
import Data.Char

import qualified Data.List as List
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Control.Monad.State as SM
import qualified FRP.Yampa as Yampa

import Text.Show.Unicode
import Control.Arrow
import Control.Monad

dump :: Show a => a -> IO ()
dump x = putStrLn (ushow x)

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
  | BotSessionHelpStart
  | BotSessionHelpFinish
  | BotStackPush    CbtMethod CbtStack String
  | BotStackFreeze  CbtMethod CbtStack
  | BotMethodStart  CbtMethod
  | BotMethodAbort  CbtMethod
  | BotMethodFinish CbtMethod
  | BotMethodColumnStart  CbtMethod CbtStack
  | BotMethodColumnAbort  CbtMethod CbtStack
  | BotMethodColumnFinish CbtMethod CbtStack
  deriving (Eq, Ord, Show)

class PrettyJP a where
  ppJP :: a -> String

data CbtStack = CbtStackAT
              | CbtStackST
              | CbtStackEM
              | CbtStackCD
              | CbtStackRR
              deriving (Eq, Ord, Show)

instance PrettyJP CbtStack where
  ppJP stack = case stack of
    CbtStackAT -> "自動思考"
    CbtStackST -> "状況"
    CbtStackEM -> "感情"
    CbtStackCD -> "認知の歪み"
    CbtStackRR -> "合理的反応"

instance PrettyJP CbtMethod where
  ppJP method = case method of
    CbtTripleColumnTechnique -> "3列法"
    CbtFiveColumnTechnique   -> "5列法"


data CbtMethod = CbtTripleColumnTechnique
               | CbtFiveColumnTechnique
               deriving (Eq, Ord, Show)

type CbtSessionEvent = Yampa.Event CbtMethod

cbtEvent :: CbtMethod -> CbtSessionEvent
cbtEvent method = Yampa.Event method

data CbtCommand
  = CbtSessionStart
  | CbtSessionAbort
  | CbtSessionFinish
  | CbtSessionHelpStart
  | CbtSessionHelpFinish
  | CbtMethodStart   CbtMethod
  | CbtMethodAbort   CbtMethod
  | CbtMethodFinish  CbtMethod
  | CbtMethodColumnStart   CbtMethod CbtStack
  | CbtMethodColumnAbort   CbtMethod CbtStack
  | CbtMethodColumnFinish  CbtMethod CbtStack
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

-- Finite state machine combinators based on
-- Event switching using Kleene's language
-- (i.e. the regular expression)

type ESF ei eo a b = ei
                     -> Yampa.SF a (b, Yampa.Event eo)

dEmpty :: ESF e e a a
dEmpty e = Yampa.identity
           &&&
           Yampa.constant (Yampa.Event e)

-- | Associative concatenation, i.e. "ab" in regexp
dStep :: ESF e e a b -> ESF e e a b -> ESF e e a b
dStep x y = \e -> x' e `Yampa.dSwitch` y
  where
    x' e = x e >>>
           Yampa.arr noev *** Yampa.identity
    noev x = (x, noEvent)


-- | Homomorphism from non-empty list. Unit is available
--   only if a -> b is inhabited.
dStepFold :: [ ESF e e a b ] -> ESF e e a b
dStepFold = foldl1 dStep

dStep' :: Yampa.SF a (b, Yampa.Event ())
       -> Yampa.SF a (b, Yampa.Event ())
       -> Yampa.SF a (b, Yampa.Event ())
dStep' x y = x' `Yampa.dSwitch` const y
   where
     x' = x >>> Yampa.arr noev *** Yampa.identity
     noev x = (x, noEvent)

dStepFold' :: [ Yampa.SF a (b, Yampa.Event ()) ]
             -> Yampa.SF a (b, Yampa.Event ())
dStepFold' = foldl1 dStep'

-- | Choice operator, i.e. "a|b". Lax (but not
--   strict) associative.
dAlt :: forall il ol ir or a b.
        ESF il ol a b
     -> ESF ir or a b
     -> ESF (Either il ir) (Either ol or) a b
dAlt x y e = case (e :: Either il ir) of
  Left  il -> x il >>>
              Yampa.identity *** Yampa.arr (fmap Left)
  Right ir -> y ir >>>
              Yampa.identity *** Yampa.arr (fmap Right)

infixr 5 `dAlt`

-- | One or more, usually denoted by "a+"
dPlus :: ESF e e a b -> ESF e e a b
dPlus x = x `dStep` dPlus x

cbtHello :: Yampa.SF BotInput [BotOutput]
cbtHello = Yampa.constant $ [ BotMessage "hello world!" ]

orP :: (a -> Bool)
    -> (a -> Bool)
    -> (a -> Bool)
orP p q x = p x || q x


type EBot i o = ESF i o BotInput [BotOutput]
type Two = Either () ()


cbt :: Yampa.SF BotInput [BotOutput]
cbt = cbtE CbtSessionStart >>^ fst

cbtE :: CbtBot
cbtE = dPlus $ cbtSessionGuideE `dStep` doMethod
  where
    doMethod e = case e of
       CbtSessionHelpStart   ->
         cbtSessionHelpE e
       CbtMethodStart CbtTripleColumnTechnique ->
         cbtTripleColumnTechniqueE e
       _  ->
         cbtUnimplementedE e

cbtUnimplementedE :: CbtBot
cbtUnimplementedE e =
  Yampa.constant $
  ([BotMessage $ "unimplemented:" ++ ushow e],
   noEvent)

type BotEvent = Yampa.Event CbtCommand

cbtSessionGuideE :: CbtBot
cbtSessionGuideE _ = Yampa.arr parse
  where
    parse :: BotInput -> ([BotOutput], BotEvent)
    parse (Just input)
      | input == ":start" =
          ([BotStart], noEvent)
      | input == "ヘルプ" =
          (helpMessage  , ev CbtSessionHelpStart )
      | input `elem` ["3", ":3", "自動思考"] =
          (tctMessage, event tctEvent)
      | otherwise =
          ([BotWarn $ "unknown command:" ++ input],
           noEvent)

    parse Nothing = ([BotFinish], noEvent)

    ev = Yampa.Event
    helpMessage =
      [ BotMessage "コマンド:"
      , BotMessage "ヘルプ -- Enter help mode"
      , BotMessage "3 -- Start Triple-column technique"
      , BotMessage "自動思考 -- Start Triple-column technique"
      ]

    tctMessage =
      [ BotMethodStart
        CbtTripleColumnTechnique
      ]
    tctEvent  =
      CbtMethodStart CbtTripleColumnTechnique

cbtSessionHelpE :: EBot CbtCommand CbtCommand
cbtSessionHelpE _ = Yampa.arr $ help
  where
    help (Just input)
      | input == "認知の歪み" =
        ( [BotMessage "AN, OG, MF, DP, MR, FT, MM, EM, SH, LB, PE"],
          noEvent )
      | otherwise =
        ([], event CbtSessionHelpFinish)
    help Nothing = ([BotSessionHelpFinish], noEvent)

type CbtBot = EBot CbtCommand CbtCommand

cbtColumnFinished :: Yampa.SF BotInput Bool
cbtColumnFinished = Yampa.arr finished
  where
    finished (Just line) =
      or [ isBlankLine line
         , line `elem` ["以上", "終わり"]
         ]
    finished (Nothing) = True

cbtColumnE :: CbtMethod -> CbtStack -> CbtBot
cbtColumnE method stack _ = let
  ev  = CbtMethodColumnFinish method stack
  evIf e True  = event e
  evIf e False = noEvent
  in
  cbtPushLine method stack
  &&&
  (cbtColumnFinished >>^ evIf ev)

cbtTripleColumnTechniqueE :: CbtBot
cbtTripleColumnTechniqueE =
  dStepFold [atMode, cdMode, rrMode]
  where
    method = CbtTripleColumnTechnique
    atMode = cbtColumnE method CbtStackAT
             `mapOnEventE` (++ cdMessage)
    cdMode = cbtColumnE method CbtStackCD
             `mapOnEventE` (++ rrMessage)
    rrMode = cbtColumnE method CbtStackRR
             `mapOnEventE` (++ tctFinishMessage)


    cdMessage =
      [ BotMessage "認知の歪み"
      , BotMessage "AN, OG, MF, DP, MR, FT, MM, EM, SH, LB, PE"
      ]
    rrMessage = [ BotMessage "合理的反応"]

    tctFinishMessage =
      [ BotMethodFinish CbtTripleColumnTechnique ]

cbt' :: Yampa.SF BotInput [BotOutput]
cbt' = cbtSession `Yampa.dSwitch` doMethod
  where
    stMessage = [ BotMessage "状況" ]
    atMessage = [ BotMessage "自動思考" ]
    emMessage = [ BotMessage "感情" ]

    cdMessage = [ BotMessage "認知の歪み"
                , BotMessage "AN, OG, MF, DP, MR, FT, MM, EM, SH, LB, PE"
                ]
    rrMessage = [ BotMessage "合理的反応"]

    doMethod method = case method of
      CbtTripleColumnTechnique ->
        dStepFold' [ cbtColumnUntil method CbtStackAT
                     (isBlankLine `orP` (== "認知の歪み"))
                     >>^ mapOnEvent (cdMessage ++)
                   , cbtColumnUntil method CbtStackCD
                     (isBlankLine `orP` (== "合理的反応"))
                     >>^ mapOnEvent (rrMessage ++)
                   , cbtColumnUntil method CbtStackRR
                     isBlankLine
                     >>^ methodFinish method
                   ]
        `Yampa.dSwitch`
        const cbt

      CbtFiveColumnTechnique ->
        dStepFold' [ cbtColumnUntil method CbtStackST
                     (isBlankLine `orP` (== "自動思考"))
                     >>^ mapOnEvent (atMessage ++)
                   , cbtColumnUntil method CbtStackAT
                     (isBlankLine `orP` (== "感情"))
                     >>^ mapOnEvent (emMessage ++)
                   , cbtColumnUntil method CbtStackEM
                     (isBlankLine `orP` (== "認知の歪み"))
                     >>^ mapOnEvent (cdMessage ++)
                   , cbtColumnUntil method CbtStackCD
                     (isBlankLine `orP` (== "合理的反応"))
                     >>^ mapOnEvent (rrMessage ++)
                   , cbtColumnUntil method CbtStackRR
                     isBlankLine
                     >>^ methodFinish method
                   ]
        `Yampa.dSwitch`
        const cbt

    methodFinish method = mapOnEvent (++ [BotMethodFinish method ])

mapOnEvent f (x, ev)
      | isEvent ev = (f x, ev)
      | otherwise  = (x, ev)

mapOnEventE :: CbtBot
            -> ([BotOutput] -> [BotOutput])
            -> CbtBot
mapOnEventE bot f = \e -> bot e >>^ mapOnEvent f

cbtSession :: Yampa.SF BotInput ([BotOutput], CbtSessionEvent)
cbtSession = proc input -> do
  case input of
    Just ":start" -> returnA -< ([BotStart], noEvent)
    Just ":help"  -> cbtSessionHelp -< ()
    Just "ヘルプ" -> cbtSessionHelp -< ()
    Just ":3" -> returnA -< ([BotMethodStart CbtTripleColumnTechnique],
                             cbtEvent CbtTripleColumnTechnique)
    Just "自動思考" -> returnA -< ([BotMethodStart CbtTripleColumnTechnique],
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
      Just "ヘルプ" -> help
      Just "" -> finish
      Just l  -> [BotStackPush   method stack l]
      Nothing -> finish

    help   = [BotMessage (ppJP stack)]
    finish = [ BotStackFreeze method stack
             , BotMethodColumnFinish method stack
             ]

type UnitEvent = Yampa.Event ()

cbtBlankLineEvent :: Yampa.SF BotInput UnitEvent
cbtBlankLineEvent = Yampa.arr ev
  where
    ev input = case input of
      Just l  -> if isBlankLine l
                 then event ()
                 else noEvent
      Nothing -> event ()

isBlankLine l = all isSpace l

cbtMatchEvent :: (String -> Bool)
              -> Yampa.SF BotInput UnitEvent
cbtMatchEvent p = Yampa.arr ev
  where
    ev input = case input of
      Just l  -> if p l
                 then event ()
                 else noEvent
      Nothing -> event ()

cbtColumnUntil :: CbtMethod
               -> CbtStack
               -> (String -> Bool)
               -> Yampa.SF BotInput ([BotOutput], Yampa.Event () )
cbtColumnUntil method stack p = cbtPushLine method stack
                                &&&
                                cbtMatchEvent p

isEvent (Yampa.Event _) = True
isEvent (Yampa.NoEvent) = False
