-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

import Html exposing (Html, div, input, li, ul, text)
import Html.App as App
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onInput)
import List exposing (filter, map)
import String exposing (contains, toLower)

type alias Entry =
  { name : String
  , login : String
  , password : String
  }

type alias Model =
  { searchString : String
  , entryNames : List String
  , currentEntry : Maybe Entry
  }

type Msg
  = SearchStringChanged String
  | EntryNamesReceived (List String)

emptyModel =
  { searchString = ""
  , entryNames = ["piet", "klaas"]
  , currentEntry = Nothing
  }

main =
  App.beginnerProgram { model = emptyModel, view = view, update = update }

update msg model =
  case msg of
    SearchStringChanged newString ->
      { model | searchString = newString }

    EntryNamesReceived names ->
      { model | entryNames = names }

filterEntries : String -> List String -> List String
filterEntries needle =
  let isMatch candidate = contains (toLower needle) (toLower candidate)
  in  filter isMatch

renderListItem entryName = li [] [ text entryName ]

view model =
  let selectedEntries = filterEntries model.searchString model.entryNames
  in
    div []
      [ input [ placeholder "type here to search", onInput SearchStringChanged ] []
      , ul [] (map renderListItem selectedEntries)
      ]
