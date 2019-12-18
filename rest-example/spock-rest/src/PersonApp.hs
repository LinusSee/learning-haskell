{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}


module PersonApp
  ( app
  ) where


import           PersonModel

import           Web.Spock
import           Web.Spock.Config

import           Data.Aeson               hiding (json)
import           Data.Text                (Text, pack)


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
