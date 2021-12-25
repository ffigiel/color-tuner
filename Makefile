.PHONY: clean dev lint
VERSION_ID := $(shell git describe --always --long --tags --dirty)

.DEFAULT_GOAL:= dist

clean:
	rm -rf dist dist.old

dist: clean
	npx vite build

dev:
	npx vite

lint:
	npx elm-review --fix-all
