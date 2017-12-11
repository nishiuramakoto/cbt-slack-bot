{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Text
import           Lib

apiKey :: Text
apiKey = "xoxp-283421821111-281832874353-283270330902-9644d36b22c20f7ae829f38e2b1fd114"

main :: IO ()
main = cbtBot apiKey
