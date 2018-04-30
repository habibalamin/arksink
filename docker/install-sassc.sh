#!/usr/bin/env bash

set -euo pipefail

apt-get install -y autoconf libtool

export SASS_LIBSASS_PATH=`pwd`/libsass

mkdir libsass
curl -L https://github.com/sass/libsass/archive/3.5.0.tar.gz |
  tar -xzf - -C libsass --strip-components=1

(cd libsass && autoreconf --force --install \
  && ./configure --disable-tests --enable-shared --prefix=/usr/local \
  && make -j4 && make install)

mkdir sassc
curl -L https://github.com/sass/sassc/archive/3.5.0.tar.gz |
  tar -xzf - -C sassc --strip-components=1

(cd sassc && autoreconf --force --install \
  && ./configure \
    --enable-shared \
    --prefix=/usr/local \
    --with-libsass=/usr/local \
  && make -j4 && make install)

rm -rf sassc && rm -rf libsass

ldconfig
