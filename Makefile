.PHONY: build run deploy clean

run:
	parcel src/index.html

build:
	parcel build src/index.html
