-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

-- This module exposes the Stanford Javascript Crypto Library to Elm.
module Sjcl exposing (EncryptedData, encrypt, decrypt)

import Json.Encode

-- Import the js functions sjcl.encrypt and sjcl.decrypt.
-- See also src/Native/Sjcl.js.
import Native.Sjcl exposing (encrypt, decrypt)

-- The json structure returned by SJCL. Could be given more structure,
-- but let's not bother for now.
type alias EncryptedData = Json.Encode.Value

encrypt : String -> String -> EncryptedData
encrypt = Native.Sjcl.encrypt

decrypt : String -> EncryptedData -> String
decrypt = Native.Sjcl.decrypt
