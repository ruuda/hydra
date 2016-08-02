-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Web.Scotty (get, json, scotty)

main = scotty 2971 $ do
  get "/ids" $ do
    let ids = ["Foo", "Bar"] :: [Text]
    json ids
