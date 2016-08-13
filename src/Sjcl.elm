-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

-- This module exposes the Stanford Javascript Crypto Library to Elm.
module Sjcl exposing (Ciphertext, Plaintext, encrypt, decrypt)

-- Import the js functions sjcl.encrypt and sjcl.decrypt.
-- See also src/Native/Sjcl.js.
import Native.Sjcl exposing (encrypt, decrypt)

type alias Ciphertext = String
type alias Plaintext = String

encrypt : String -> Plaintext -> Ciphertext
encrypt = Native.Sjcl.encrypt

decrypt : String -> Ciphertext -> Plaintext
decrypt = Native.Sjcl.decrypt
