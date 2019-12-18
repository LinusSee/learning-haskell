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
import           PersonModel

import           Web.Spock
import           Web.Spock.Config

import           Data.Aeson               hiding (json)
import           Data.Monoid              ((<>))
import           Data.Text                (Text, pack)
import           GHC.Generics

import           Control.Monad.Logger     (LoggingT, runStdoutLoggingT)
import           Database.Persist         hiding (get, delete)
import qualified Database.Persist         as P
import           Database.Persist.Sqlite  hiding (get, delete)
import           Database.Persist.TH



main :: IO ()
main = do
  pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
  spockConfig <- defaultSpockCfg () (PCPool pool) ()
  runStdoutLoggingT  $ runSqlPool (runMigration migrateAll) pool
  runSpock 8080 (spock spockConfig app)

app :: Api
app = do
  get "people" $ do
    allPeople <- getPeople
    json allPeople
  get ("people" <//> var) $ \personId -> do
    maybePerson <- getPerson personId
    case maybePerson of
      Nothing -> errorJson 2 "Could not find a person with a matching id"
      Just thePerson -> json thePerson
  post "people" $ do
    maybePerson <- jsonBody :: ApiAction (Maybe Person)
    case maybePerson of
      Nothing -> errorJson 1 "Failed to parse request body as a person"
      Just thePerson -> do
        newId <- insertPerson thePerson
        json $ object ["result" .= String "success", "id" .= newId]
  put ("people" <//> var) $ \personId -> do
    maybePerson <- jsonBody :: ApiAction (Maybe Person)
    case maybePerson of
      Nothing -> errorJson 1 "Failed to parse request body as a person"
      Just thePerson -> do
        result <- repsertPerson personId thePerson
        json result
  delete ("people" <//> var) $ \personId -> do
    result <- deletePerson personId
    json result


errorJson:: Int -> Text -> ApiAction ()
errorJson code message = json $
  object
  [ "result" .= String "failure"
  , "error"  .= object ["code" .= code, "message" .= message]
  ]
