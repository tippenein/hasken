Hasken
----

[![Build Status](https://travis-ci.org/tippenein/hasken.svg?branch=master)](https://travis-ci.org/tippenein/hasken)

Hasken you shall receive

Easily searchable local document store with option to push to server for backup

Your documents are titled, tagged and stored locally.

## Setup

`stack install`

`cp hasken.yml.sample ~/.hasken.yml`


## Usage

```
hasken add shia-magic shia,gif,magic,whatever http://reactiongifs.me/wp-content/uploads/2013/08/shia-labeouf-magic-gif.gif
```

For an explanation of commands

```
hasken add --help                                                                                                                                                                                                    bmo@bmos-MacBook-Air-4
Usage: hasken add Title Tag1,Tag2 Content and stuff
  Add a document to the local storage

  Available options:
    -h,--help                Show this help text
```

And searchable from the commandline

```
hasken search magic
> shia-magic -> http://reactiongifs.me/wp-content/uploads/2013/08/shia-labeouf-magic-gif.gif
   ~~|  shia gif magic whatever
```

Syncing to/from server

```
hasken sync
```

use it for:
  - storing links to gifs
  - recipes
  - notes
  - etc..

## Help

```
$ hasken -h
create and tag documents for searchable recall

Usage: hasken [--version] [--show-tags] [COMMAND]
  tagged local storage with a sync option

Available options:
  -h,--help                Show this help text
  --version                Show version and exit
  --show-tags              show tags when displaying documents

Available commands:
  add                      Add a document to the local storage
  search                   search the local docs
  list                     list local docs (defaults to limit 10)
  sync                     sync with the remote
  serve                    serve an instance of the remote component
```

## Config

```yaml
# file: ~/.hasken.yml
remote:
  host: somesite.com
  port: 8082
  userKey: <some guid>
local:
  showTags: false
```

Feel free to run your own instance of the server with `hasken serve` or connect to a public one with these settings
```
  domain: deltadrome.us
  port: 8009
```

### remote:
The `sync` option will only push items which are not present on the server, and pull items
the server has exclusively.

### local:
The `showTags` yaml attribute can be overridden by the `--show-tags` flag in any command that displays documents
