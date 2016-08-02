-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

import Html exposing (Html, div, input, li, ul, text)
import Html.App as App
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onInput)
import Http
import Json.Decode as Json
import List exposing (filter, map)
import String exposing (contains, toLower)
import Task

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

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

emptyModel =
  { searchString = ""
  , entryNames = ["piet", "klaas"]
  , currentEntry = Nothing
  }

init : (Model, Cmd Msg)
init = (emptyModel, getEntryNames)

-- UPDATE

type Msg
  = SearchStringChanged String
  | EntryNamesReceived (List String)
  | EntryNamesFailed Http.Error

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SearchStringChanged newString ->
      ({ model | searchString = newString }, Cmd.none)

    EntryNamesReceived names ->
      ({ model | entryNames = names }, Cmd.none)

    EntryNamesFailed error ->
      -- TODO: Display a message? Retry?
      (model, Cmd.none)

-- VIEW

filterEntries : String -> List String -> List String
filterEntries needle =
  let
    isMatch candidate = contains (toLower needle) (toLower candidate)
  in
    filter isMatch

renderListItem : String -> Html Msg
renderListItem entryName = li [] [ text entryName ]

view : Model -> Html Msg
view model =
  let
    selectedEntries = filterEntries model.searchString model.entryNames
  in
    div []
      [ input [ placeholder "type here to search", onInput SearchStringChanged ] []
      , ul [] (map renderListItem selectedEntries)
      ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- HTTP

getEntryNames : Cmd Msg
getEntryNames =
  let
    -- TODO: Fix urls and port numbers
    getRequest = Http.get decodeEntryNames "/api/entries"
  in
    Task.perform EntryNamesFailed EntryNamesReceived getRequest

decodeEntryNames : Json.Decoder (List String)
decodeEntryNames =
  Json.list Json.string
