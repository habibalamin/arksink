.SILENT: database setup migrate build assets clean-assets run-server run-client server-repl client-repl new-migration
.PHONY: database setup migrate build assets clean-assets run-server run-client server-repl client-repl new-migration

GHCI_OPTIONS = --ghci-options -XOverloadedStrings --ghci-options -XScopedTypeVariables

missing_arg = "Missing required argument '$(1)': make $@ $(1)=$(2)"

setup: build migrate

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
	sassc -t compressed -m auto \
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

database:
	# OPTIMIZE: load environment with dotenv once.
	# - Create database if non-existent.
	# - Create migrations table if non-existent.
	database_exists=$$(stack exec dotenv -- -o -f .envdefaults -f .env \
	  "psql -t -A -c \
	    \"SELECT true FROM pg_database WHERE datname = 'arksink'\"") && \
	if [ ! "$$database_exists" = "t" ]; then \
	  stack exec dotenv -- -o -f .envdefaults -f .env \
	    'createdb $$ARKSINK_DB_NAME' ; \
	fi
	stack exec dotenv -- -o -f .envdefaults -f .env \
	  'psql -d $$ARKSINK_DB_NAME -c \
	    "CREATE TABLE IF NOT EXISTS migrations (id VARCHAR(255) PRIMARY KEY)"' \
	  2>/dev/null >/dev/null
	stack exec dotenv -- -o -f .envdefaults -f .env \
	  'pg_dump --schema-only --no-owner $$ARKSINK_DB_NAME > db/arksink.sql'

migrate: database
	# - Create temporary directory.
	# - Store applied migrations in temporary file.
	# - Store all applicable migrations in temporary file.
	# - Get applicable migrations that aren't applied.
	# - Apply unapplied migrations.
	# - Dump updated schema.
	tmpdir=$$(mktemp -d) && \
	stack exec dotenv -- -o -f .envdefaults -f .env \
	  'psql -d $$ARKSINK_DB_NAME -t -A -c "SELECT id FROM MIGRATIONS"'\
	  > "$$tmpdir/applied-migrations" && \
	find db/migrations -depth 1 \
	  | sed -e 's:^db/migrations/::' -e 's:\.sql$$::' \
	  > "$$tmpdir/migrations" && \
	unapplied_migrations=$$(diff \
	  --changed-group-format='%>' --unchanged-group-format='' \
	  "$$tmpdir/applied-migrations" "$$tmpdir/migrations") || \
	for migration in $$unapplied_migrations; do \
	  echo "=== Applying migration: $${migration}"; \
	  stack exec dotenv -- -o -f .envdefaults -f .env \
	    'psql -d $$ARKSINK_DB_NAME -f \
	      db/migrations/'"$$migration"'.sql' && \
	  stack exec dotenv -- -o -f .envdefaults -f .env \
	    'psql -d $$ARKSINK_DB_NAME -c \
	      "INSERT INTO migrations VALUES ('"'$$migration'"')"' \
	    >/dev/null ; \
	done
	stack exec dotenv -- -o -f .envdefaults -f .env \
	  'pg_dump --schema-only --no-owner $$ARKSINK_DB_NAME > db/arksink.sql'
