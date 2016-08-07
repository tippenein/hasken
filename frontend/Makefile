all: build js
gen:
	twirec -e Document -i . --elm-out src/Gen --elm-version 0.17

build:
	elm package install

js:
	elm make src/Main.elm --output=elm.js
