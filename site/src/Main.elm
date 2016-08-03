import Http as Http
import Json.Decode as Json -- exposing (..)
import Task exposing (..)
import Html.App exposing (..)
import Html exposing (Html, div)

import Document exposing (..)
import Component exposing (..)

main : Program Never
main = Html.App.program
  { init = model ! [fetchDocuments]
  , update = update
  , view = view
  , subscriptions = \_ -> Sub.none
  }

type alias Model =
  { documents : List Document
  , message : String
  , queryString : String
  }

userKey = "b4be5a63-eb40-439b-a2f3-ff480bd87884"

model : Model
model =
  { documents = []
  , message = ""
  , queryString = ""
  }

searchDocuments : String -> Cmd Action
searchDocuments q =
    let url = Http.url ("http://localhost:8099/documents/" ++ userKey) [ ("q", q) ]
    in
      Http.get (Json.list jdecDocument) url
        |> Task.mapError toString
        |> Task.perform ErrorOccurred DocumentsFetched

fetchDocuments : Cmd Action
fetchDocuments =
  Http.get (Json.list jdecDocument) ("http://localhost:8099/documents/" ++ userKey)
    |> Task.mapError toString
    |> Task.perform ErrorOccurred DocumentsFetched

-- UPDATE

type Action
  = NoOp
  | FetchDocuments
  | Search String
  | ErrorOccurred String
  | DocumentsFetched (List Document)

update : Action -> Model -> (Model, Cmd Action)
update action model =
  case action of
    Search term ->
      { model | message = "searching " ++ term} ! [searchDocuments term]
    NoOp ->
      model ! []
    FetchDocuments ->
      { model | message = "Initiating data fetch!" } ! [fetchDocuments]
    ErrorOccurred errorMessage ->
      { model | message = "Oops! An error occurred: " ++ errorMessage } ! []
    DocumentsFetched documents ->
      { model | documents = documents, message = "The data has been fetched!" } ! []

-- VIEW

view : Model -> Html Action
view model =
  div []
    [
      statusMessage model
    , searchBox model Search
    , documentList model.documents
    ]
