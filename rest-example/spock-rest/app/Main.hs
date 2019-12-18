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
import           Database.Persist.Sqlite  hiding (get)
import           Database.Persist.TH


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person json
  name Text
  age Int
  deriving Show
|]


type Api = SpockM SqlBackend () () ()
type ApiAction a = SpockAction SqlBackend () () a


main :: IO ()
main = do
  pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
  spockConfig <- defaultSpockCfg () (PCPool pool) ()
  runStdoutLoggingT  $ runSqlPool (runMigration migrateAll) pool
  runSpock 8080 (spock spockConfig app)

app :: Api
app = do
  get "people" $ do
    allPeople <- runSQL $ selectList [] [Desc PersonId]
    json allPeople
  get ("people" <//> var) $ \personId -> do
    maybePerson <- runSQL $ P.get personId :: ApiAction (Maybe Person)
    case maybePerson of
      Nothing -> errorJson 2 "Could not find a person with a matching id"
      Just thePerson -> json thePerson
  post "people" $ do
    maybePerson <- jsonBody :: ApiAction (Maybe Person)
    case maybePerson of
      Nothing -> errorJson 1 "Failed to parse request body as a person"
      Just thePerson -> do
        newId <- runSQL $ insert thePerson
        json $ object ["result" .= String "success", "id" .= newId]


runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn


errorJson:: Int -> Text -> ApiAction ()
errorJson code message = json $
  object
  [ "result" .= String "failure"
  , "error"  .= object ["code" .= code, "message" .= message]
  ]
