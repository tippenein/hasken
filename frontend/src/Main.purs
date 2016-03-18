module Main where

import Control.Monad.Eff.Console

type Document =
  { title :: String
  , content :: String
  , tags :: Array String
  }

main = log "Hello, World!"
