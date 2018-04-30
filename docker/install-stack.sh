#!/usr/bin/env bash

set -euo pipefail

apt-get install -y \
  g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev

curl -L https://github.com/commercialhaskell/stack/releases/download/v1.6.5/stack-1.6.5-linux-x86_64.tar.gz \
  | tar -xzf -

cp stack-1.6.5-linux-x86_64/stack /usr/local/bin/stack

rm -rf stack-1.6.5-linux-x86_64-static
