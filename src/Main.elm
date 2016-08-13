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
import Task exposing (Task, andThen, mapError)

import Entry as Entry
import Entry exposing (Entry)

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

type alias Model =
  { searchString : String
  , entryNames : List String
  , currentEntry : Maybe Entry
  , editEntry : Entry
  }

emptyModel : Model
emptyModel =
  { searchString = ""
  , entryNames = ["piet", "klaas"]
  , currentEntry = Nothing
  , editEntry = Entry.empty
  }

init : (Model, Cmd Msg)
init = (emptyModel, getEntryNames)

-- UPDATE

type Msg
  = SearchStringChanged String
  | EntryNamesReceived (List String)
  | EntryNamesFailed Http.Error
  | EntryEditorMsg Entry.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SearchStringChanged newString ->
      ({ model | searchString = newString }, Cmd.none)

    EntryNamesReceived names ->
      ({ model | entryNames = names }, Cmd.none)

    EntryNamesFailed reason ->
      -- TODO: Display a message? Retry?
      (model, Cmd.none)

    EntryEditorMsg msg ->
      let
        (updatedModel, entryCmd, outMsg) =
          Entry.update saveEntry msg model.editEntry
        innerCmd = Cmd.map EntryEditorMsg entryCmd
        cmd =
          case outMsg of
            -- If saving was successful, reload the list of entries, because a
            -- new one might have been added.
            Just Entry.EntrySaved -> Cmd.batch [ innerCmd, getEntryNames ]
            Nothing -> innerCmd
      in
        ({ model | editEntry = updatedModel }, cmd)

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
      , div [] [ App.map EntryEditorMsg (Entry.view model.editEntry) ]
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
    getRequest = Http.get decodeEntryNames "/api/entries"
  in
    Task.perform EntryNamesFailed EntryNamesReceived getRequest

decodeEntryNames : Json.Decoder (List String)
decodeEntryNames =
  Json.list Json.string

saveEntry : Entry -> Task Http.Error ()
saveEntry entry =
  let
    url = "/api/entries/" ++ (Http.uriEncode entry.name)
    bodyJson = Entry.encrypt "hard-coded-key" entry
    body = Http.string (Json.Encode.encode 0 bodyJson)
    rawTask = Http.send Http.defaultSettings
      { verb = "PUT"
      , headers = [ ("Content-Type", "application/json") ]
      , url = url
      , body = body
      }
    toHttpError rawError =
      case rawError of
        Http.RawTimeout -> Http.Timeout
        Http.RawNetworkError -> Http.NetworkError
    verifyOk response =
      if response.status == 200
        then
          Task.succeed ()
        else
          let
            reason =
              case response.value of
                Http.Text message -> message
                Http.Blob _ -> "cannot make sense of server error message"
          in
            Task.fail (Http.BadResponse response.status reason)
  in
    (mapError toHttpError rawTask) `andThen` verifyOk
