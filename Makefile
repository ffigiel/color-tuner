.PHONY: clean dev lint

.DEFAULT_GOAL:= dist

clean:
	rm -rf .cache dist

dist: clean
	npx parcel build --no-source-maps src/index.html

dev:
	npx parcel src/index.html

lint:
	npx elm-analyse
