{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}


module API where


import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (throwE)
import           Data.Int (Int64)
import           Data.Proxy (Proxy(..))
import           Database.Persist (Key, Entity)
import           Database.Persist.Postgresql (ConnectionString)
import           Network.Wai.Handler.Warp (run)
import           Servant.API
import           Servant.Client
import           Servant.Server

import           Database (fetchUserPG, createUserPG, fetchPostgresConnection, fetchRedisConnection, fetchRedisPool, cacheUser, fetchUserRedis)
import           Database (PGInfo, RedisInfo)
import           Database.Redis (Connection)
import           Schema


type UsersAPI =
       "users" :> Capture "userid" Int64 :> Get '[JSON] User
  :<|> "users" :> ReqBody '[JSON] User   :> Post '[JSON] Int64

usersAPI :: Proxy UsersAPI
usersAPI = Proxy :: Proxy UsersAPI

fetchUsersHandler :: PGInfo -> Connection -> Int64 -> Handler User
fetchUsersHandler connString connection uid = do
  maybeUserRedis <- liftIO $ fetchUserRedis connection uid
  --maybeUserRedis <- liftIO $ fetchUserPG connString uid
  case maybeUserRedis of
    Just user -> return user
    Nothing -> do
      maybeUser <- liftIO $ fetchUserPG connString uid
      case maybeUser of
        Just user -> liftIO (cacheUser connection uid user) >> return user
        Nothing -> Handler (throwE $ err401 { errBody = "Could not find a user with ID" })

createUserHandler :: PGInfo -> User -> Handler Int64
createUserHandler connString user = liftIO $ createUserPG connString user


usersServer :: PGInfo -> Connection -> Server UsersAPI
usersServer pgInfo redisConnection =
  fetchUsersHandler pgInfo redisConnection :<|>
  createUserHandler pgInfo

runServer :: IO ()
runServer = do
  pgInfo <- fetchPostgresConnection
  redisInfo <- fetchRedisConnection
  connection <- fetchRedisPool redisInfo
  run 8000 (serve usersAPI (usersServer pgInfo connection))
