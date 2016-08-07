module Markdown where

import Servant.HTML.Lucid (HTML)

homePage :: Html ()
homePage =
  doctypehtml_ $ do
    head_ $ do
      title_ "Hasken"
      metas
      stylesheets
      script_ [src_ "assets/elm.js"] ""
    body_ (script_ "var elmApp = Elm.fullscreen(Elm.Main)")

stylesheets =
  mapM (\link ->
    link_ [ rel_ "stylesheet"
          , href_ link
          ]) links
  where
    links = [ "//cdnjs.cloudflare.com/ajax/libs/skeleton/2.0.4/skeleton.min.css"
            , "//maxcdn.bootstrapcdn.com/font-awesome/4.3.0/css/font-awesome.min.css"
            , "http://fonts.googleapis.com/css?family=Lato"
            , "assets/css/default.css"
            ]

metas =
  meta_ [ name_ "viewport"
        , content_ "width=device-width, initial-scale=1" ]

    -- <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
    -- <meta http-equiv="X-UA-Compatible" content="chrome=1">
