#!/usr/bin/env bash

set -euo pipefail

version='4.0.9'

# Download
curl -O http://download.redis.io/releases/redis-${version}.tar.gz

# Extract
tar -xzf redis-${version}.tar.gz

# Verify
echo "df4f73bc318e2f9ffb2d169a922dec57ec7c73dd07bccf875695dbeecd5ec510\
  redis-${version}.tar.gz" | sha256sum -c -

# Build & install
(cd redis-${version} && make && make install)

# Clean
rm -rf redis-${version} redis-${version}.tar.gz
