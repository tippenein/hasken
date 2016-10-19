module Component exposing (..)

import Html.Attributes exposing (..)
import Html exposing (Html, Attribute, div, input, text)
import Html.Events exposing (onInput)
import Regex as Regex

type Either a b
    = Left a
    | Right b


statusMessage msg =
    if msg == ""
    then
        div [] []
    else
        div [class "warning-box"] [ text msg ]

documentList docs =
    case docs of
        [] -> div [] []
        _ ->
          let chunked = List.sortWith preferLink docs

          in
            div [ ] (List.map documentListElement chunked)

preferLink a b =
    case ((isImage a.content), (isImage b.content)) of
        (True, False) -> GT
        (False, True) -> LT
        _             -> EQ

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
isImage c = Regex.contains (Regex.regex "(jpg|gif|png|:large)$") c

userKeyInput model action =
  let styl = if activated then "" else "hidden"
      activated = model.userKeyFocus == "" && model.queryString == ""
  in
    Html.input [
          class ("u-full-width search-box " ++ styl)
        , type' "search"
        , placeholder "user key"
        , onInput action
        , value model.userKeyFocus
      ]
      []

searchBox model action =
  let styl = if activated then "" else "hidden"
      activated = model.userKeyFocus /= "" -- && List.isEmpty model.documents
  in
    Html.input [
          class ("u-full-width search-box " ++ styl)
        , type' "search"
        , placeholder "search.."
        , onInput action
        , value model.queryString
      ]
      []

genericInput model_attr action t p =
  Html.input [
        class "u-full-width search-box"
      , type' t
      , placeholder p
      , onInput action
      , value model_attr
    ]
    []
