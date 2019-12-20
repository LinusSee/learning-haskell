module Main where

import Database (localConnString, migrateDB)
import API (runServer)

main :: IO ()
main = do
  migrateDB localConnString
  runServer 
