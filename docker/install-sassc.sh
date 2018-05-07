#!/usr/bin/env bash

set -euo pipefail

version='3.5.0'

# Install dependencies
apt-get install -y autoconf libtool

export SASS_LIBSASS_PATH=`pwd`/libsass

# Stage libsass
mkdir libsass

# Download libsass
curl -Lo libsass-${version}.tar.gz \
  https://github.com/sass/libsass/archive/${version}.tar.gz

# Verify libsass
echo "9d499927329c7f8816ab67c571fb7cc0ea21d38c6f47c7c58c98de99ef7645eb\
  libsass-${version}.tar.gz" | sha256sum -c -

# Extract libsass
tar -xzf libsass-${version}.tar.gz -C libsass --strip-components=1

# Build & install libsass
(cd libsass && autoreconf --force --install \
  && ./configure --disable-tests --enable-shared --prefix=/usr/local \
  && make -j4 && make install)

# Stage sassc
mkdir sassc

# Download sassc
curl -Lo sassc-${version}.tar.gz \
  https://github.com/sass/sassc/archive/${version}.tar.gz

# Verify libsass
echo "26f54e31924b83dd706bc77df5f8f5553a84d51365f0e3c566df8de027918042\
  sassc-${version}.tar.gz" | sha256sum -c -

# Extract sassc
tar -xzf sassc-${version}.tar.gz -C sassc --strip-components=1

# Build & install sassc
(cd sassc && autoreconf --force --install \
  && ./configure \
    --enable-shared \
    --prefix=/usr/local \
    --with-libsass=/usr/local \
  && make -j4 && make install)

# Clean libsass & sassc
rm -rf libsass libsass-${version}.tar.gz sassc sassc-${version}.tar.gz

ldconfig
