import Html exposing (Html, Attribute, div, input, text)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)
import Maybe as Maybe

main =
  Html.beginnerProgram { model = model, view = view, update = update }

-- genericDecode decoder payload =
--   case decodeValue decoder payload of
--     Ok val -> val
--     Err message -> genericError message

-- -- TODO: fix this
-- genericError msg = {}

-- decodeDocuments : Value -> List Document
-- decodeDocuments payload = genericDecode documentsDecoder payload

-- decodeDocument : Value -> Document
-- decodeDocument payload = genericDecode documentDecoder payload


-- documentsDecoder : Decoder (List Document)
-- documentsDecoder = Json.Decode.list documentDecoder

-- documentDecoder : Decoder Document
-- documentDecoder =
--   succeed Document
--     |: ("name" := string)
--     |: ("id" := int)

-- MODEL

type alias Document =
  { id : Int
  , title : String
  , content : String
  , tags : List String
  }

type alias Model =
  { documents : List Document
  }

model : Model
model =
  { documents = fetchDocuments }

fetchDocuments : List Document
fetchDocuments = --- decodeDocuments """[ {name: whatever, id: 1}, {name: derp, id: 2}]"""
  [Document 1 "whatever" "blah blah" ["derp"], Document 2 "derp" "stuff" ["tag1", "tag2"]]

-- UPDATE

type Msg
  = Search String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Search term ->
      { model | documents = List.filter (\document -> String.contains term document.title) fetchDocuments }

-- VIEW

documentList ds =
  Html.ul [ documentListStyle ] (List.map documentListElement ds)

documentListStyle =
  style
    [ ("list-style", "none")
    ]

documentListElement d = Html.li [] [text d.title]

view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "search documents", onInput Search ] []
    , documentList model.documents
    ]
