{-# LANGUAGE OverloadedStrings #-}


module Main where

import Data.IORef
import Data.Text          (Text)
import Web.Spock
import Web.Spock.Config

import Lucid
import Web.Spock.Lucid    (lucid)


type Server a = SpockM () () ServerState a

newtype ServerState = ServerState { notes :: IORef [Note] }

data Note = Note {
    author :: Text
  , contents :: Text }


app :: Server ()
app = get root $ lucid $ do
  h1_ "Hello!"
  p_  "How are you today?"

main :: IO ()
main = do
  st <- ServerState <$>
    newIORef [ Note "Alice" "Must not forget to walk dog."
      , Note "Bob" "Must. Eat. Pizza!!"
    ]
  cfg <- defaultSpockCfg () PCNoDatabase st
  runSpock 8080 (spock cfg app)
