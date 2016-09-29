import Http as Http
import Json.Decode as Json
import Task exposing (..)
import Html.App exposing (..)
import Html exposing (Html, div)
import Document exposing (..)
import Component exposing (..)

main : Program Never
main = Html.App.program
  { init = model ! []
  , update = update
  , view = view
  , subscriptions = \_ -> Sub.none
  }

type alias Model =
  { documents : Either String (List Document)
  , queryString : String
  , userKeyFocus : String
  }

model : Model
model =
  { documents = Right []
  , userKeyFocus = ""
  , queryString = ""
  }

baseUrl = "http://deltadrome.us/hasken/api"

searchDocuments : String -> String -> Cmd Action
searchDocuments m q =
    if q == "" then getDocs m Nothing else getDocs m (Just [ ("q", q) ])

fetchDocuments : String -> Cmd Action
fetchDocuments uk =
    getDocs uk Nothing

getDocs : String -> Maybe (List (String, String)) -> Cmd Action
getDocs uk mquery_params =
    let url = case mquery_params of
                  Just q -> Http.url docUrl q
                  Nothing -> Http.url docUrl []
        docUrl = baseUrl ++ "/documents/" ++ uk
    in
      Http.get (Json.list jdecDocument) url
        |> Task.mapError toString
        |> Task.perform ErrorOccurred DocumentsFetched

-- UPDATE

type Action
  = NoOp
  | FetchDocuments
  | InputUserKey String
  | Search String
  | ErrorOccurred String
  | DocumentsFetched (List Document)

update : Action -> Model -> (Model, Cmd Action)
update action model =
  case action of
    Search term ->
      { model | queryString = term} ! [searchDocuments model.userKeyFocus term]
    NoOp ->
      model ! []
    InputUserKey u ->
      { model | userKeyFocus = u } ! [fetchDocuments u]
    FetchDocuments ->
      model ! [fetchDocuments model.userKeyFocus]
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
    , userKeyInput model InputUserKey
    , documentList model.documents
    ]
