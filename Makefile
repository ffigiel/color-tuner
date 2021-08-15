.PHONY: clean dev lint
VERSION_ID := $(shell git describe --always --long --tags --dirty)

.DEFAULT_GOAL:= dist

clean:
	rm -rf dist dist.old

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
	npx snowpack build
	./postBuild.sh

dev:
	npx snowpack dev

lint:
	npx elm-review --fix-all
