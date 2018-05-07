#!/usr/bin/env bash

set -euo pipefail

apt-get update \
  && apt-get install -y curl gnupg2 apt-transport-https

# Verify repo
curl -sSO https://dl.yarnpkg.com/debian/pubkey.gpg
echo "710d786f708ac411a96731dc7d979806afb6da14ff284fc9f1a97f5c050be8ce\
  pubkey.gpg" | sha256sum -c -
apt-key add pubkey.gpg && rm pubkey.gpg

# Add repo to sources
echo 'deb https://dl.yarnpkg.com/debian/ stable main' |
  tee /etc/apt/sources.list.d/yarn.list

apt-get update
