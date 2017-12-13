{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Text
import           Lib

apiKey :: Text
apiKey = undefined

main :: IO ()
main = cbtBot apiKey
