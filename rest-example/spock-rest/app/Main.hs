{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module Main where

import           Lib

import           Web.Spock
import           Web.Spock.Config

import           Data.Aeson               hiding (json)
import           Data.Monoid              ((<>))
import           Data.Text                (Text, pack)
import           GHC.Generics

import           Control.Monad.Logger     (LoggingT, runStdoutLoggingT)
import           Database.Persist         hiding (get)
import qualified Database.Persist         as P
import           Database.Persist.TH


data Person = Person
    { name :: Text
    , age  :: Int
    } deriving (Generic, Show)

instance ToJSON Person
instance FromJSON Person


type Api = SpockM () () () ()
type ApiAction a = SpockAction () () () a


main :: IO ()
main = do
  spockConfig <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockConfig app)

app :: Api
app = do
  get "people" $
    json [ Person { name="Fry", age=25 }, Person { name="Bender", age=4 } ]
  post "people" $ do
    thePerson <- jsonBody' :: ApiAction Person
    text $ "Parsed: " <> pack (show thePerson)
