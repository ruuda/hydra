-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Entry exposing
  ( EncryptedEntry
  , Entry
  , Model
  , Msg
  , OutMsg (EntrySaved)
  , empty
  , encrypt
  , update
  , view
  )

import Json.Encode as Json
import Html exposing (Html, button, div, input, label, text)
import Html.Events exposing (onClick, onInput)
import Http
import Task
import Task exposing (Task)

import Sjcl exposing (encrypt)

-- MODEL

type alias Entry =
  { name : String
  , login : String
  , password : String
  }

type alias Model = Entry

empty : Entry
empty =
  { name = ""
  , login = ""
  , password = ""
  }

-- UPDATE

type Msg
  = NameChanged String
  | LoginChanged String
  | PasswordChanged String
  | SaveClicked
  | SaveSucceeded
  | SaveFailed Http.Error

type OutMsg
  = EntrySaved

type alias SaveEntryFn = Entry -> Task Http.Error ()

update : SaveEntryFn -> Msg -> Model -> (Model, Cmd Msg, Maybe OutMsg)
update saveEntry msg model =
  case msg of
    NameChanged newName ->
      ({ model | name = newName }, Cmd.none, Nothing)

    LoginChanged newLogin ->
      ({ model | login = newLogin }, Cmd.none, Nothing)

    PasswordChanged newPassword ->
      ({ model | password = newPassword }, Cmd.none, Nothing)

    SaveClicked ->
      let
        handleFailure err = SaveFailed err
        handleSuccess _ = SaveSucceeded
        cmd = Task.perform handleFailure handleSuccess (saveEntry model)
      in
        (model, cmd, Nothing)

    SaveFailed reason ->
      -- TODO: Retry? Display failure to the user?
      (model, Cmd.none, Nothing)

    SaveSucceeded ->
      -- TODO: Give success feedback to the user?
      (model, Cmd.none, Just EntrySaved)

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ label []
      [ text "Name"
      , input [onInput NameChanged] []
      ]
    , label []
      [ text "Login"
      , input [onInput LoginChanged] []
      ]
    , label []
      [ text "Password"
      , input [onInput PasswordChanged] []
      ]
    , button [ onClick SaveClicked ] [ text "Save" ]
    ]

-- ENCRYPTION

type alias EncryptedEntry = Json.Value

encrypt : String -> Entry -> EncryptedEntry
encrypt key entry =
  Json.object
    [ ("name", Json.string entry.name)
    , ("login", Json.string (Sjcl.encrypt key entry.login))
    , ("password", Json.string (Sjcl.encrypt key entry.password))
    ]
