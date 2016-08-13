-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

import Html exposing (Html, button, div, h2, input, label, li, ul, text)
import Html.App as App
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Json
import Json.Encode
import List exposing (filter, map)
import String exposing (contains, toLower)
import Task

-- Native import, see src/Native/Sjcl.js
import Native.Sjcl exposing (encrypt, decrypt)

main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- Wrapper functions for the Stanford Javascript Crypto Library.

-- The json structure returned by SJCL. Could be given more structure,
-- but let's not bother for now.
type alias EncryptedData = Json.Encode.Value

encrypt : String -> String -> EncryptedData
encrypt = Native.Sjcl.encrypt

decrypt : String -> EncryptedData -> String
decrypt = Native.Sjcl.decrypt

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

emptyModel : Model
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
  | AddNewEntryClicked
  | EntryNamesReceived (List String)
  | EntryNamesFailed Http.Error

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SearchStringChanged newString ->
      ({ model | searchString = newString }, Cmd.none)

    AddNewEntryClicked ->
      -- TODO: Deal with new entry form.
      (model, Cmd.none)

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
      [ h2 [] [ text "Add new entry" ]
      , label []
        [ text "Name"
        , input [] []
        ]
      , label []
        [ text "Login"
        , input [] []
        ]
      , label []
        [ text "Password"
        , input [] []
        ]
      , button [ onClick AddNewEntryClicked ] [ text "Add" ]
      , h2 [] [ text "Browse entries" ]
      , input [ placeholder "type here to search", onInput SearchStringChanged ] []
      , ul [] (map renderListItem selectedEntries)
      ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

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
