{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows              #-}
module Lib (
  cbtBot,
  echoBot
  ) where

import           Fsm
import qualified FRP.Yampa as Yampa

import           Control.Lens       hiding ((.=))
import           Control.Monad
import           Control.Monad.State.Strict(StateT)
import qualified Control.Monad.State.Strict as State
import           Control.Monad.IO.Class
import           Control.Arrow
import           Data.Aeson
import qualified Data.HashMap.Lazy  as HML (lookup)
import           Data.Monoid
import           Data.Text
import qualified Data.List  as List
import qualified Data.Maybe as Maybe
import           Data.Time.Clock

import           GHC.Generics
import           Network.URL
import           Network.WebSockets
import           Network.Wreq       as Wreq
import           Text.Printf
import           Text.Show.Unicode
import           Wuss

data StartResponse =
  StartResponse {ok  :: Bool
                ,url :: String}
  deriving (Eq,Show,Generic,FromJSON)

data Presence =
  Active
  deriving (Eq,Show,Generic,FromJSON)

data SlackMessage
  = Hello
  | Reconnect {reconnectUrl :: Text}
  | Unknown Object
  | Message {body    :: Text
            ,channel :: Text}
  | PresenceChange {user     :: Text
                   ,presence :: Presence}
  | Warning Text
  | Disconnect
  deriving (Eq,Show,Generic)

instance FromJSON SlackMessage where
  parseJSON (Object o) =
    case HML.lookup "type" o of
      Just (String "hello") -> return Hello
      Just (String "message") ->
        do body <- o .: "text"
           channel <- o .: "channel"
           return Message {..}
      Just (String "reconnect_url") ->
        do reconnectUrl <- o .: "url"
           return Reconnect {..}
      Just (String "presence_change") ->
        do user <- o .: "user"
           presence <- o .: "presence"
           return PresenceChange {..}
      _ -> return (Unknown o)
  parseJSON _ = fail "Unrecognised message."

startEndpoint :: String
startEndpoint = "https://slack.com/api/rtm.start"

startSession :: Text -> IO StartResponse
startSession apiKey =
  do r <-
       getWith (defaults & param "token" .~ [apiKey]) startEndpoint >>= asJSON
     return $ r ^. responseBody

runBotApp :: Text -- ^ api key
       -> (Connection -> IO ()) -- ^ slack bot
       -> IO () -- ^ result
runBotApp apiKey bot =
  do session <- startSession apiKey
     let Just wsurl = importURL $ url session
     let Absolute urlHost = url_type wsurl
     let hostname = host urlHost
     let path = url_path wsurl
     printf "Connecting to: %s %s\n" hostname path
     runSecureClient hostname
                     443
                     ("/" ++ path)
                     bot

echoBot apiKey = runBotApp apiKey echoBotApp
cbtBot  apiKey = runBotApp apiKey cbtBotApp

echoBotApp :: Connection -> IO ()
echoBotApp connection =
  do putStrLn "====\nConnected\n===="
     forever $
       do Text dataMessage <- receiveDataMessage connection
          let msg :: Maybe SlackMessage = decode dataMessage
          print msg
          case msg of
            Just (Message t ch) ->
              sendTextData connection $
              encode $
              object ["id"    .= (1 :: Int)
                     ,"text" .= ("I heard you say: " <> t)
                     ,"channel" .= ch
                     ,"type" .= pack "message"]
            _ -> return ()

cbtBotApp :: Connection -> IO ()
cbtBotApp connection = sfBotApp connection cbtSF

sendMessage :: Connection -- ^ connection
            -> Text  -- ^ message to be sent
            -> Text  -- ^ channel
            -> IO () -- ^ the action
sendMessage connection t ch =
  sendTextData connection $
  encode $
  object ["id"   .= (1 :: Int)
         ,"text" .= ("bot: " <> t)
         ,"channel" .= ch
         ,"type" .= pack "message"
         ]

type Reactor a = StateT UTCTime IO a
type ReactorInput  = SlackMessage
type ReactorOutput = [SlackMessage]

homo f x = join (fmap f x)

toSlackMessages :: Text -- ^ channel
                -> [BotOutput] -- ^ outputs from bot
                -> [SlackMessage] -- ^ result list
toSlackMessages ch outs = homo f outs
  where
    f :: BotOutput -> [SlackMessage]
    f x = case x of
        BotMessage x          ->
          [Message (pack x) ch]

        BotWarn x             ->
          [Message (pack $ "warning:" ++  x) ch]

        BotMethodStart  method ->
          [Message (pack $ ppJP method) ch]

        BotMethodFinish method ->
          [ Message (pack $ ppJP method ++"終了") ch
          ]

        BotAbort ->
          [ Message (pack "中断") ch
          , Disconnect
          ]

        BotMethodColumnStart method stack ->
          [Message (pack $ ppJP stack) ch]
        BotMethodColumnFinish method stack ->
          [Message (pack $ ppJP stack ++ "終了") ch]

        BotStackFreeze  method stack ->
          [Message (pack $ ppJP stack ++ "終了") ch]
        s            -> [Warning (pack $ ushow s)]

cbtSF :: Yampa.SF SlackMessage [SlackMessage]
cbtSF = proc msg -> do
  case msg of
    Message body ch -> do
      outs <- cbt -< Just (unpack body)
      returnA -< toSlackMessages ch outs

    _ -> returnA -< []

sfBotApp :: Connection
         -> Yampa.SF SlackMessage [SlackMessage]
         -> IO ()
sfBotApp connection sf = do
  curr_time <- getCurrentTime
  flip State.evalStateT curr_time $
    Yampa.reactimate init sensor actuate sf

  where
    updateTime :: Reactor Yampa.DTime
    updateTime = do
      prev_time   <- State.get
      curr_time   <- liftIO getCurrentTime
      State.put curr_time
      return $ realToFrac $ curr_time `diffUTCTime` prev_time

    init :: Reactor ReactorInput
    init = liftIO $ do
      putStrLn "====\nConnected\n===="
      return Hello

    sensor :: Bool -> Reactor (Yampa.DTime, Maybe ReactorInput)
    sensor may_block
      | may_block || True = do
          dtime <- updateTime
          Text dataMessage <- liftIO $ receiveDataMessage connection
          let msg :: Maybe SlackMessage
              msg = decode dataMessage
          liftIO $ uprint msg
          return (dtime, msg)

      | not may_block = do
          dtime <- updateTime
          liftIO $ uprint "no blocking"
          return (dtime, Nothing)

    actuate :: Bool -> ReactorOutput -> Reactor Bool
    actuate changed outputs = do
      liftIO $ uprint outputs
      bs <- mapM actuate1 outputs
      return $ List.any (== True) bs -- FIXME

      where
        actuate1 out = case out of
          Message body ch  -> do
            liftIO $ sendMessage connection body ch
            return False
          Warning w -> do
            liftIO $ putStrLn $ "warning:" ++ unpack w
            return False
          Disconnect  -> do
            return True
