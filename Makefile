.PHONY: clean dev lint
VERSION_ID := $(shell git describe --always --long --tags --dirty)

.DEFAULT_GOAL:= dist

clean:
	rm -rf .cache dist

deploy-pages: dist
	git checkout dd674c0
	git branch -D gh-pages
	git checkout -b gh-pages
	mv dist/* .
	git add index.*
	git commit -m "chore: release ${VERSION_ID}"
	git push -f origin gh-pages
	git checkout main

dist: clean
	npx parcel build \
		--no-source-maps \
		--public-url /color-tuner/ \
		src/index.html

dev:
	npx parcel src/index.html

lint:
	npx elm-analyse
