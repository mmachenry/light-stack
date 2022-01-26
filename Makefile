.PHONY: build run test

run:
	parcel src/index.html

test:
	elm-test

build:
	parcel build src/index.html
