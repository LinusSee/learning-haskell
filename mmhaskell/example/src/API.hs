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

import           Database (fetchUserPG, createUserPG, fetchPostgresConnection, fetchRedisConnection, cacheUser, fetchUserRedis)
import           Database (PGInfo, RedisInfo)
import           Schema


type UsersAPI =
       "users" :> Capture "userid" Int64 :> Get '[JSON] User
  :<|> "users" :> ReqBody '[JSON] User   :> Post '[JSON] Int64

usersAPI :: Proxy UsersAPI
usersAPI = Proxy :: Proxy UsersAPI

fetchUsersHandler :: PGInfo -> RedisInfo -> Int64 -> Handler User
fetchUsersHandler connString redisInfo uid = do
  maybeUserRedis <- liftIO $ fetchUserRedis redisInfo uid
  case maybeUserRedis of
    Just user -> return user
    Nothing -> do
      maybeUser <- liftIO $ fetchUserPG connString uid
      case maybeUser of
        Just user -> liftIO (cacheUser redisInfo uid user) >> return user
        Nothing -> Handler (throwE $ err401 { errBody = "Could not find a user with ID" })

createUserHandler :: PGInfo -> User -> Handler Int64
createUserHandler connString user = liftIO $ createUserPG connString user


usersServer :: PGInfo -> RedisInfo -> Server UsersAPI
usersServer pgInfo redisInfo =
  fetchUsersHandler pgInfo redisInfo :<|>
  createUserHandler pgInfo

runServer :: IO ()
runServer = do
  pgInfo <- fetchPostgresConnection
  redisInfo <- fetchRedisConnection
  run 8000 (serve usersAPI (usersServer pgInfo redisInfo))
