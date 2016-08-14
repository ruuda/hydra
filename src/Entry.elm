-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Entry exposing
  ( Entry
  , Model
  , Msg
  , OutMsg (EntrySaved)
  , decodeAndDecrypt
  , empty
  , encryptAndEncode
  , update
  , view
  )

import Json.Decode
import Json.Decode exposing ((:=))
import Json.Encode
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

-- ENCODE & ENCRYPT

-- Encrypts the string using the key, then encodes the result as a json string.
encodeEncryptedString : String -> String -> Json.Encode.Value
encodeEncryptedString key str =
  Json.Encode.string (Sjcl.encrypt key str)

-- A json decoder that reads a string and decrypts it using the key.
decodeEncryptedString : String -> Json.Decode.Decoder String
decodeEncryptedString key =
  -- TODO: Handle decryption failure.
  Json.Decode.map (Sjcl.decrypt key) Json.Decode.string

-- The idea of serialization here is that an Entry is never encrypted, but when
-- encoded as json it is *always* encrypted, so it is not possible to send an
-- entry over the wire unencrypted (unless it is serialized manually).

encryptAndEncode : String -> Entry -> Json.Encode.Value
encryptAndEncode key entry =
  Json.Encode.object
    [ ("name", Json.Encode.string entry.name)
    , ("login", encodeEncryptedString key entry.login)
    , ("password", encodeEncryptedString key entry.password)
    ]

decodeAndDecrypt : String -> Json.Decode.Decoder Entry
decodeAndDecrypt key =
  Json.Decode.object3 Entry
    ("name" := Json.Decode.string)
    ("login" := decodeEncryptedString key)
    ("password" := decodeEncryptedString key)
