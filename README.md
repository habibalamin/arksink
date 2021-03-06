Arksink — bookmark management for the indecisive
================================================

Arksink is a bookm[ark sync]ing solution for people who use multiple browsers. It consists of a web app with a UI & API, and a synchronising client daemon.

It may also be necessary to create browser extensions to deal with tags in browsers without native tag support for bookmarks.

Currently, only the server has been implemented, and it is incomplete.

## Getting Started

### Dependencies

All of these packages are available on Homebrew (macOS), Debian, and FreeBSD, except where noted.

- You'll need [Haskell Stack](https://www.haskellstack.org) to install the Haskell dependencies.
- Arksink is backed by the [PostgreSQL](https://www.postgresql.org) database for persistence.
- [Redis](https://redis.io) is required to store client sessions and other cache data.
- We use [Yarn](https://yarnpkg.com) to manage asset dependencies.
- [SassC](https://github.com/sass/sassc) is required to compile the Sass stylesheets.
- [Elm](http://elm-lang.org) is used for the web UI to provide interactivity and make RESTful requests. FreeBSD doesn't carry this in their package repositories (yet), but you can install it with Yarn.
- [CoffeeScript](http://coffeescript.org) is required to compile the thin Elm-mounting code to JS. This may be removed in a future date if we can get away with using Elm for everything and keeping the JS code to mount-only.

### Running the server

First, you need to create the database, then apply the migrations. All you need to do is run the `migrate` task, which will automatically create the database if it doesn't exist yet:

```
make migrate
```

You'll need to have a Redis server running with all the default settings:

```
redis-server 2>&1 >/dev/null &
```

If you installed Redis using Homebrew on OS X, you can use the services interface:

```
brew services start redis
```

Before you can run the server, you may need to create an environment file at `.env` and fill in the database credentials:

```
ARKSINK_DB_USERNAME="$USER"
ARKSINK_DB_PASSWORD=''
```

Finally, you can run the server using the default settings by running the run-server Make task:

```
make run-server
```

Go to localhost:3000 to see the web interface.

You'll need to set up an SSL/TLS certificate and a secure Nginx proxy if you want to log in. Session cookies are restricted to HTTPS. You can generate the certificate and key with this command:

```
openssl req -x509 -nodes -days 365 -newkeyrsa:2048 -keyout arksink.key -out arksink.cert
```

Here's a snippet of Nginx configuration to get you started with the proxy:

```
http {
  upstream arksink.tls {
    server localhost:3000;
  }

  server {
    listen      443 ssl;
    listen      [::]:443 ssl;
    server_name arksink.tls.localhost;

    ssl on;

    ssl_certificate      /usr/local/etc/ssl/arksink.cert;
    ssl_certificate_key  /usr/local/etc/ssl/arksink.key;

    location / {
      try_files $uri @$name;
    }

    location @arksink.tls {
      proxy_pass       http://arksink.tls;
      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
      proxy_set_header X-Forwarded-Proto $scheme;
      proxy_set_header Host $http_host;
      proxy_redirect   off;
    }
  }
}
```

With a self-signed certificate, your browser should warn you that it's untrusted because it doesn't know the signer; you know the signer (itself) and you presumably trust it — since you created it — so you can add an exception for that certificate.

### Developing

You can create new migrations for the server with an automatic timestamp by running the new-migration task:

```
make new-migration name=a_migration
```

Then, you can apply the new migration individually:

```
psql -d arksink -f db/migrations/19700101000000_a_migration.sql
```

The client is currently unimplemented.
