database_exists=$(stack exec dotenv -- -o -f .env \
  "psql -t -A -U $ARKSINK_DB_USERNAME -h $ARKSINK_DB_HOST -c \
    \"SELECT true FROM pg_database WHERE datname = '$ARKSINK_DB_NAME'\"")

if [ "$database_exists" = "t" ]; then
  stack exec dotenv -- -o -f .env \
    'dropdb -U $ARKSINK_DB_USERNAME -h $ARKSINK_DB_HOST $ARKSINK_DB_NAME'
else
  stack exec dotenv -- -o -f .env \
    'createdb -O $ARKSINK_DB_USERNAME -U $ARKSINK_DB_USERNAME -h $ARKSINK_DB_HOST $ARKSINK_DB_NAME'
fi
