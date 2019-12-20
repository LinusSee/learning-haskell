{-# LANGUAGE OverloadedStrings #-}

module Database where

import Control.Monad.Logger (runStdoutLoggingT, MonadLogger, LoggingT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.IO.Class (MonadIO)
import Database.Persist (selectList, (==.), (<.), SelectOpt(..), Entity)
import Database.Persist.Postgresql (ConnectionString, withPostgresqlConn, runMigration, SqlPersistT)

import Schema


localConnString :: ConnectionString
localConnString = "host=127.0.0.1 port=5432 user=postgres dbname=postgres"

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action =
  runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend -> runReaderT action backend

migrateDB :: ConnectionString -> IO ()
migrateDB connString = runAction connString (runMigration migrateAll)
