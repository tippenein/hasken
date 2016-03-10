Hasken
----

[![Build Status](https://travis-ci.org/tippenein/hasken.svg?branch=master)](https://travis-ci.org/tippenein/hasken)

Hasken you shall receive

Easily searchable local document store with option to push to server for backup

use it for:
  - storing links to gifs
  - recipes
  - notes
  - store your pins and passwords (once clientside encryption is implemented)

all tagged and searchable from the commandline

##Setup

`stack install`

`cp hasken.yml.sample ~/.hasken.yml`

##Usage

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
local:
  showTags: false
```

###remote:
The `sync` option will only push items which are not present on the server, and pull items
the server has exclusively.

###local:
The `showTags` yaml attribute can be overridden by the `--show-tags` flag in any command that displays documents


