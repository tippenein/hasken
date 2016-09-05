import Http as Http
import Json.Decode as Json
import Task exposing (..)
import Html.App exposing (..)
import Html exposing (Html, div)
import Document exposing (..)
import Component exposing (..)

main : Program Never
main = Html.App.program
  { init = model ! [fetchDocuments model]
  , update = update
  , view = view
  , subscriptions = \_ -> Sub.none
  }

type alias Model =
  { documents : Either String (List Document)
  , queryString : String
  , userKeyFocus : String
  }

userKey = "b4be5a63-eb40-439b-a2f3-ff480bd87884"

model : Model
model =
  { documents = Right []
  , userKeyFocus = ""
  , queryString = ""
  }

baseUrl = "http://localhost:8080"

searchDocuments : Model -> String -> Cmd Action
searchDocuments m q =
    if q == "" then getDocs m Nothing else getDocs m (Just [ ("q", q) ])

fetchDocuments : Model -> Cmd Action
fetchDocuments m =
    getDocs m Nothing

getDocs : Model -> Maybe (List (String, String)) -> Cmd Action
getDocs model mquery_params =
    let url = case mquery_params of
                  Just q -> Http.url docUrl q
                  Nothing -> Http.url docUrl []
        docUrl = baseUrl ++ "/documents/" ++ model.userKeyFocus
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
      { model | queryString = term} ! [searchDocuments model term]
    NoOp ->
      model ! []
    InputUserKey u ->
      { model | userKeyFocus = u } ! []
    FetchDocuments ->
      model ! [fetchDocuments model]
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
