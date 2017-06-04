.SILENT: build assets clean-assets run-server run-client new-migration
.PHONY: build assets clean-assets run-server run-client new-migration

GHCI_OPTIONS = --ghci-options -XOverloadedStrings --ghci-options -XScopedTypeVariables

missing_arg = "Missing required argument '$(1)': make $@ $(1)=$(2)"

build: assets
	# FIXME: dotenv ignores blank environment variables.
	stack exec dotenv -- -o -f .envdefaults -f .env 'stack build'

assets: clean-assets
	echo 'Compiling assets!'
	yarn install
	elm make --warn \
	         --output public/assets/scripts/arksink.elm.js \
	         `find src/lib/assets/scripts/elm -name '*.elm' -exec echo -n '{} ' ';' | sed 's/ $$//'`
	coffee -c --no-header \
	       -o public/assets/scripts \
	       -- `find src/lib/assets/scripts -name '*.coffee' -exec echo -n '{} ' ';' | sed 's/ $$//'`
	mkdir -p public/assets/styles
	sassc -t compressed -m \
	      -I node_modules \
	      -- src/lib/assets/styles/arksink.scss public/assets/styles/arksink.css

clean-assets:
	echo 'Cleaning assets!'
	rm -rf public/assets

run-server: build
	stack exec dotenv -- -o -f .envdefaults -f .env 'stack exec arksink-server'

run-client: build
	stack exec dotenv -- -o -f .envdefaults -f .env 'stack exec arksink'

client-repl:
	stack exec dotenv -- -o -f .envdefaults -f .env 'stack ghci $(GHCI_OPTIONS) --main-is Arksink:exe:arksink-client'

server-repl:
	stack exec dotenv -- -o -f .envdefaults -f .env 'stack ghci $(GHCI_OPTIONS) --main-is Arksink:exe:arksink-server'

new-migration:
	if [ -n "$(name)" ]; then \
	  touch db/migrations/`date +'%Y%m%d%H%M%S'`_$(name).sql; \
	else \
	  echo $(call missing_arg,name,migration_name); \
	fi
