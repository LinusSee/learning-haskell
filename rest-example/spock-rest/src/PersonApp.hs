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

import Network.HTTP.Types.Status

import           Data.Aeson               hiding (json)
import           Data.Text                (Text, pack)


app :: Api
app = do
  personGet
  peopleGet
  personPost
  personPut
  personDelete



personGet = get "people" $ do
  allPeople <- getPeople
  json allPeople

peopleGet = get ("people" <//> var) $ \personId -> do
  maybePerson <- getPerson personId
  case maybePerson of
    Nothing -> do
      setStatus notFound404
      errorJson 2 "Could not find a person with a matching id"
    Just thePerson -> json thePerson

personPost = post "people" $ do
      maybePerson <- jsonBody :: ApiAction (Maybe Person)
      case maybePerson of
        Nothing -> do
          setStatus badRequest400
          errorJson 1 "Failed to parse request body as a person"
        Just thePerson -> do
          newId <- insertPerson thePerson
          setStatus created201
          json $ object ["result" .= String "success", "id" .= newId]

personPut = put ("people" <//> var) $ \personId -> do
      maybePerson <- jsonBody :: ApiAction (Maybe Person)
      case maybePerson of
        Nothing -> do
          setStatus badRequest400
          errorJson 1 "Failed to parse request body as a person"
        Just thePerson -> do
          repsertPerson personId thePerson
          setStatus created201

personDelete = delete ("people" <//> var) $ \personId ->
      deletePerson personId



errorJson:: Int -> Text -> ApiAction ()
errorJson code message = json $
  object
  [ "result" .= String "failure"
  , "error"  .= object ["code" .= code, "message" .= message]
  ]
