-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Entry exposing (Entry, Model, Msg, empty, update, view)

import Html exposing (Html, button, div, input, label, text)
import Html.Events exposing (onClick, onInput)

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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NameChanged newName ->
      ({ model | name = newName }, Cmd.none)

    LoginChanged newLogin ->
      ({ model | login = newLogin }, Cmd.none)

    PasswordChanged newPassword ->
      ({ model | password = newPassword }, Cmd.none)

    SaveClicked ->
      -- TODO: Deal with it.
      (model, Cmd.none)

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
