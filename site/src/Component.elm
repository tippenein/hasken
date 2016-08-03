module Component exposing (..)

import Html.Attributes exposing (..)
import Html exposing (Html, Attribute, div, input, text)
import Html.Events exposing (onInput)
import String
import Regex as Regex

statusMessage model = div [] [ text model.message ]

documentList ds =
    let chunked = List.sortWith preferLink ds

    in
      div [ ] (List.map documentListElement chunked)

preferLink a b =
    case ((isImage a.content), (isImage b.content)) of
        (True, False) -> GT
        (True, True) -> EQ
        (False, False) -> EQ
        (False, True) -> LT


titleStyle =
  style
    [ ("color", "#494949")
    , ("font-size", "20px")
    ]


documentLinkStyle =
  style
    [ -- ("display", "inline-block")
    ]

documentImageStyle =
  style
    [ ("float", "left")
    , ("white-space", "nowrap")
    ]


documentListElement d =
    let styl = if isImage(d.content) then documentImageStyle else documentLinkStyle
    in
      Html.div [ styl ] [
          div [ titleStyle ]
            [text d.title ]
        , div []
            [ decorateContent d ]
      ]

decorateContent d =
    if isUrl(d.content) && isImage(d.content) then Html.img [src d.content] [text d.title]
    else if isUrl(d.content) then Html.a [href d.content] [text d.content]
    else text d.content

isUrl c = Regex.contains (Regex.regex "^https?://") c
isImage c = Regex.contains (Regex.regex "(jpg|gif|png)$") c

searchBox model action =
  Html.input [
        type' "search"
      , placeholder "search.."
      , onInput action
      , value model.queryString
    ]
    []
