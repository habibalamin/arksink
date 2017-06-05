Arksink
=======

Bookmark management for the indecisive.

## Getting Started

### Running the server

First, you need to create the database:

```
createdb arksink
```

Then, apply the migrations. Since we don't track applied migrations yet, or have any Make tasks to run unapplied ones, it takes a bit of manual work.

If you're running them on a fresh database, you can get away with a simple loop that indiscriminately applies them in order:

```
for migration in db/migrations/*; do psql -d arksink -f "$migration"; done
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
