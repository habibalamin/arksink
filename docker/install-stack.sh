#!/usr/bin/env bash

set -euo pipefail

version='1.6.5'
dist="stack-${version}-linux-x86_64"

# Install dependencies
apt-get install -y \
  g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev

# Download
curl -LO "https://github.com/commercialhaskell/stack/releases/download/v${version}/${dist}.tar.gz"

# Verify
echo "9efc933d23d7065e1787f688fc294fc37a0653e79cfa7e2008b65deef0699760\
  ${dist}.tar.gz" | sha256sum -c -
tar -xzf ${dist}.tar.gz

# Install
cp ${dist}/stack /usr/local/bin/stack

# Clean
rm -rf ${dist} ${dist}.tar.gz
