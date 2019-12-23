{-# LANGUAGE OverloadedStrings #-}

module Database where

import           Control.Monad (void)
import           Control.Monad.Logger (runStdoutLoggingT, MonadLogger, LoggingT)
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.IO.Class (MonadIO)
import           Data.ByteString.Char8 (pack, unpack)
import           Data.Int (Int64)
import           Database.Persist (get, insert, delete)
import           Database.Persist.Sql (fromSqlKey, toSqlKey)
import           Database.Persist.Postgresql (ConnectionString, withPostgresqlConn, runMigration, SqlPersistT)
import           Database.Redis (Connection, ConnectInfo, connect, Redis, runRedis, defaultConnectInfo, setex)
import qualified Database.Redis as Redis


import Schema


type PGInfo = ConnectionString
type RedisInfo = ConnectInfo


localConnString :: PGInfo
localConnString = "host=127.0.0.1 port=5432 user=postgres dbname=postgres password=admin"

-- IO because in a prod application it would be read from e.g. a config file
fetchPostgresConnection :: IO PGInfo
fetchPostgresConnection = return localConnString

fetchRedisConnection :: IO RedisInfo
fetchRedisConnection = return defaultConnectInfo

fetchRedisPool :: RedisInfo -> IO Connection
fetchRedisPool = connect


runAction :: PGInfo -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action =
  runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend -> runReaderT action backend

runRedisAction :: Connection -> Redis a -> IO a
runRedisAction = runRedis


migrateDB :: PGInfo -> IO ()
migrateDB connString = runAction connString (runMigration migrateAll)

fetchUserPG :: PGInfo -> Int64 -> IO (Maybe User)
fetchUserPG connString uid = runAction connString (get (toSqlKey uid))

createUserPG :: PGInfo -> User -> IO Int64
createUserPG connString user = fromSqlKey <$> runAction connString (insert user)

deleteUserPG :: PGInfo -> Int64 -> IO ()
deleteUserPG connString uid = runAction connString (delete userKey)
  where userKey :: Key User
        userKey = toSqlKey uid


cacheUser :: Connection -> Int64 -> User -> IO ()
cacheUser redisPool uid user = runRedisAction redisPool $ void $ setex byteId 3600 byteUser
  where byteId = pack . show $ uid
        byteUser = pack . show $ user

fetchUserRedis :: Connection -> Int64 -> IO (Maybe User)
fetchUserRedis redisPool uid = runRedisAction redisPool $ do
  result <- Redis.get . pack . show $ uid
  case result of
    Right (Just userString) -> return $ Just (read . unpack $ userString)
    _ -> return Nothing
