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
  { documents : Either String (List Document)
  , queryString : String
  }

userKey = "b4be5a63-eb40-439b-a2f3-ff480bd87884"

model : Model
model =
  { documents = Right []
  , queryString = ""
  }

baseUrl = "http://deltadrome.us/hasken/api"

searchDocuments : String -> Cmd Action
searchDocuments q =
    if q == "" then getDocs Nothing else getDocs (Just [ ("q", q) ])

fetchDocuments : Cmd Action
fetchDocuments =
    getDocs Nothing

getDocs : Maybe (List (String, String)) -> Cmd Action
getDocs mquery_params =
    let url = case mquery_params of
                  Just q -> Http.url docUrl q
                  Nothing -> Http.url docUrl []
        docUrl = baseUrl ++ "/documents/" ++ userKey
    in
      Http.get (Json.list jdecDocument) url
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
      { model | queryString = term} ! [searchDocuments term]
    NoOp ->
      model ! []
    FetchDocuments ->
      model ! [fetchDocuments]
    ErrorOccurred errorMessage ->
      { model | documents = Left("Oops! An error occurred: " ++ errorMessage) } ! []
    DocumentsFetched documents ->
      { model | documents = Right(documents) } ! []

-- VIEW

view : Model -> Html Action
view model =
  div []
    [
      statusMessage model.documents
    , searchBox model Search
    , documentList model.documents
    ]
