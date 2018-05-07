#!/usr/bin/env bash

set -euo pipefail

version='v8.11.1'
host="https://nodejs.org/dist/${version}"

# Install dependencies
apt-get install -y python build-essential

# Download
curl -O "${host}/node-${version}.tar.gz"
curl -O "${host}/SHASUMS256.txt"
curl -O "${host}/SHASUMS256.txt.sig"

# Verify
for key in 94AE36675C464D64BAFA68DD7434390BDBE9B9C5 \
           FD3A5288F042B6850C66B31F09FE44734EB7990E \
           71DCFD284A79C3B38668286BC97EC7A07EDE3FC1 \
           DD8F2338BAE7501E3DD5AC78C273792F7D83545D \
           C4F0DFFF4E8C1A8236409D08E73BC641CC11F4C8 \
           B9AE9905FFD7803F25714661B63B535A4C206CA9 \
           56730D5401028683275BD23C23EFEFE93C4CFFFE \
           77984A986EBC2AA786BC0F66B01FBB92821C587A; do
  gpg --keyserver ipv4.pool.sks-keyservers.net --recv-keys $key
done
gpg --verify SHASUMS256.txt.sig SHASUMS256.txt
grep node-${version}.tar.gz SHASUMS256.txt | sha256sum -c -

# Extract
tar -xzf node-${version}.tar.gz

# Build & install
(cd node-${version} && ./configure --without-npm && make && make install)

# Clean
rm -rf node-${version} node-${version}.tar.gz \
  SHASUMS256.txt SHASUMS256.txt.sig
