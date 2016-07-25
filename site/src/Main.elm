import Html exposing (Html, Attribute, div, input, text)
import Html.App as Html
import Http as Http
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String
import Json.Decode as Json -- exposing (..)
import Json.Decode.Extra exposing (..)
import Maybe as Maybe
import Document exposing (..)
import Task exposing (..)
import Html.App exposing (..)

main = Html.App.program
  { init = model ! [fetchDocuments]
  , update = update
  , view = view
  , subscriptions = \_ -> Sub.none
  }

type alias Model =
  { documents : List Document
  , message : String
  }

userKey = "b4be5a63-eb40-439b-a2f3-ff480bd87884"

model : Model
model =
  { documents = [], message = "" }

fetchDocuments : Cmd Action
fetchDocuments =
  Http.get (Json.list jdecDocument) ("http://localhost:8080/documents/" ++ userKey)
    |> Task.mapError toString
    |> Task.perform ErrorOccurred DocumentsFetched

-- UPDATE

type Action
  = NoOp
  | FetchDocuments
  | ErrorOccurred String
  | DocumentsFetched (List Document)
  -- | Search String

update : Action -> Model -> (Model, Cmd Action)
update action model =
  case action of
    -- Search term ->
    --   { model | documents = List.filter (\document -> String.contains term document.title || String.contains term document.content) fetchDocuments }
    NoOp ->
      model ! []
    FetchDocuments ->
      { model | message = "Initiating data fetch!" } ! [fetchDocuments]
    ErrorOccurred errorMessage ->
      { model | message = "Oops! An error occurred: " ++ errorMessage } ! []
    DocumentsFetched documents ->
      { model | documents = documents, message = "The data has been fetched!" } ! []

-- VIEW

documentList ds =
  Html.ul [ documentListStyle ] (List.map documentListElement ds)

documentListStyle =
  style
    [ ("list-style", "none")
    ]

documentListElement d = Html.li [] [text (d.title ++ " - " ++ d.content ++ " | ")]

view : Model -> Html Action
view model =
  div []
    [
      div [] [ text model.message ]
    , Html.button [ onClick FetchDocuments ] [ text "search documents" ]
    , documentList model.documents
    ]
