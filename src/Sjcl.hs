-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Sjcl (EncryptedData, emptyEncryptedData) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- Mirrors the structure of the data produced by SJCL, the Stanford Javascript
-- Crypto Library. The fields themselves are not important on the server: all
-- encryption and decryption is done at the client. The server simply manages
-- storage, so there is an opaque Sjcl type.
data EncryptedData = EncryptedData
  { iv :: Text
  , v :: Int
  , iter :: Int
  , ks :: Int
  , ts :: Int
  , mode :: Text
  , adata :: Text
  , cipher :: Text
  , salt :: Text
  , ct :: Text
  } deriving (Generic)

instance FromJSON EncryptedData
instance ToJSON EncryptedData

emptyEncryptedData :: EncryptedData
emptyEncryptedData = EncryptedData
  { iv = ""
  , v = 0
  , iter = 0
  , ks = 0
  , ts = 0
  , mode = "none"
  , adata = ""
  , cipher = "none"
  , salt = ""
  , ct = ""
  }
