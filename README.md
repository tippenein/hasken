Hasken
----

Hasken you shall receive

Easily searchable local document store with option to push to server for backup

use it for:
  - storing links to gifs
  - recipes
  - notes
  - store your pins and passwords (once clientside encryption is implemented)

all tagged and searchable from the commandline

##Goal

This document store should function without an internet connection.
When a connection is present, it should push new data to a server for backup storage.

##Setup

within the sandbox
`cabal configure; cabal build; cabal install`

user package install (puts binary in `~/.cabal/bin`)
- `runhaskell Setup.hs configure --user`
- `runhaskell Setup.hs build`
- `runhaskell Setup.hs install`

search for something
`./hasken search query`

add something
`./hasken add title tag1,tag2,tag3 content blah blah blah`

list the last 10 docs
`./hasken`
