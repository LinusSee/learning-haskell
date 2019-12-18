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

module PersonModel where


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

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person json
  name Text
  age Int
  deriving Show
|]

type Api = SpockM SqlBackend () () ()
type ApiAction a = SpockAction SqlBackend () () a

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn



getPerson :: PersonId -> ApiAction (Maybe Person)
getPerson personId = runSQL $ P.get personId


getPeople :: ApiAction [Entity Person]
getPeople = runSQL $ selectList [] [Desc PersonId]


insertPerson :: Person -> ApiAction PersonId
insertPerson person = runSQL $ insert person


repsertPerson :: PersonId -> Person -> ApiAction ()
repsertPerson personId person = runSQL $ P.repsert personId person


deletePerson :: PersonId -> ApiAction ()
deletePerson personId = runSQL $ P.delete personId
