-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar, newTVar, readTVar, readTVarIO)
import Control.Monad (mfilter, when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (readFile, writeFile)
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Types.Status (notFound404)
import Prelude hiding (readFile, writeFile)
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import Web.Scotty (file, get, json, jsonData, param, put, scotty, status, text)

import qualified Data.Aeson as Aeson
import qualified Data.Set as Set

type Ciphertext = Text

data Entry = Entry
  { name :: Text
  , login :: Ciphertext
  , password :: Ciphertext
  } deriving (Generic)

instance Aeson.FromJSON Entry
instance Aeson.ToJSON Entry where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

-- Define entry identity based on the name.
instance Eq Entry where
  x == y = name x == name y

instance Ord Entry where
  compare x y = compare (name x) (name y)

-- Creates an entry with the given name but invalid login and password,
-- to be used for searching the set of entries.
entryWithName :: Text -> Entry
entryWithName entryName = Entry
  { name = entryName
  , login = ""
  , password = ""
  }

lookupEntry :: Text -> Set Entry -> Maybe Entry
lookupEntry entryName entries =
  let isEqual entry = (name entry) == entryName
  in  mfilter isEqual $ Set.lookupLE (entryWithName entryName) entries

persistEntries :: FilePath -> Set Entry -> IO ()
persistEntries fname entries = writeFile fname (Aeson.encode entries)

loadEntries :: FilePath -> IO (Set Entry)
loadEntries fname = do
  exists <- doesFileExist fname
  if exists then do
    jsonEntries <- readFile fname
    case Aeson.eitherDecode' jsonEntries of
      Left parseErr -> do
        putStrLn "Vault has been damaged, it could not be loaded:"
        putStrLn parseErr
        exitFailure -- Terminate the program with failure exit code.
      Right entries ->
        return entries
  else do
    putStrLn "No existing vault found, using empty one."
    return Set.empty

serve :: TVar (Set Entry) -> (Set Entry -> IO ()) -> IO ()
serve entriesVar persistCallback = scotty 2971 $ do

  get "/" $
    file "static/index.html"

  get "/main.js" $
    file "static/main.js"

  get "/sjcl.js" $
    file "static/sjcl.js"

  get "/api/entries" $ do
    entries <- liftIO $ readTVarIO entriesVar
    let names = fmap name $ Set.toList entries
    json names

  put "/api/entries/:name" $ do
    -- Parse request body as an entry.
    entry <- jsonData

    -- The body includes the name already, but to be a good REST api we put the
    -- name in the url too. They must be equal.
    nameParam <- param "name"
    when (name entry /= nameParam) $ fail "Entry name in url does not match name in body."

    -- Insert the new entry (will overwrite if it exists), and read the current
    -- snapshot.
    entries <- liftIO $ atomically $ do
      modifyTVar entriesVar $ Set.insert entry
      readTVar entriesVar

    -- Persist the updated vault. TODO: This can race, it should be handled by
    -- a queue or inside a lock.
    liftIO $ persistCallback entries

    -- Respond with a simple text message.
    text "OK"

  get "/api/entries/:name" $ do
    nameParam <- param "name"
    entries <- liftIO $ readTVarIO entriesVar
    case lookupEntry nameParam entries of
      Just entry -> json entry
      Nothing    -> status notFound404 >> text "No such entry."

main :: IO ()
main = do
  entries <- loadEntries "vault.json"
  entriesVar <- atomically $ newTVar entries
  serve entriesVar (persistEntries "vault.json")
