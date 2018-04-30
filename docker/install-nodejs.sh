#!/usr/bin/env bash

set -euo pipefail

apt-get install -y python build-essential

curl https://nodejs.org/dist/v8.11.1/node-v8.11.1.tar.gz | tar -xzf -
(cd node-v8.11.1 && ./configure --without-npm && make && make install)
rm -rf node-v8.11.1
