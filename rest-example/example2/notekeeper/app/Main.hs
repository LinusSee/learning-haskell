{-# LANGUAGE OverloadedStrings #-}


module Main where

import Data.IORef
import Data.Text              (Text)
import Data.Semigroup         ((<>))
import Web.Spock
import Web.Spock.Config
import Control.Monad          (forM_)
import Control.Monad.IO.Class (liftIO)

import Lucid
import Web.Spock.Lucid        (lucid)


type Server a = SpockM () () ServerState a

newtype ServerState = ServerState { notes :: IORef [Note] }

data Note = Note {
    author :: Text
  , contents :: Text }


app :: Server ()
app = do
  get root $ do
    notes' <- getState >>= (liftIO . readIORef . notes)
    lucid $ do
      h1_ "Notes"
      ul_ $ forM_ notes' $ \note -> li_ $ do
        toHtml (author note)
        ": "
        toHtml (contents note)
      h2_ "New Note"
      form_ [method_ "post"] $ do
        label_ $ do
          "Author: "
          input_ [name_ "author"]
        label_ $ do
          "Contents: "
          textarea_ [name_ "contents"] ""
        input_ [type_ "submit", value_ "Add Note"]
  post root $ do
    author <- param' "author"
    contents <- param' "contents"
    notesRef <- notes <$> getState
    liftIO $ atomicModifyIORef' notesRef $ \notes ->
      (notes <> [Note author contents], ())
    redirect "/"

main :: IO ()
main = do
  st <- ServerState <$>
    newIORef [ Note "Alice" "Must not forget to walk dog."
      , Note "Bob" "Must. Eat. Pizza!!"
    ]
  cfg <- defaultSpockCfg () PCNoDatabase st
  runSpock 8080 (spock cfg app)
