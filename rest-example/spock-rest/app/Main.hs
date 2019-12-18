{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}


module Main where


import           PersonModel              (migrateAll)
import           PersonApp

import           Web.Spock
import           Web.Spock.Config

import           Control.Monad.Logger     (LoggingT, runStdoutLoggingT)
import           Database.Persist.Sqlite  hiding (get, delete)



main :: IO ()
main = do
  pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
  spockConfig <- defaultSpockCfg () (PCPool pool) ()
  runStdoutLoggingT  $ runSqlPool (runMigration migrateAll) pool
  runSpock 8080 (spock spockConfig app)
