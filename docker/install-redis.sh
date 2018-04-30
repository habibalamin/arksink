#!/usr/bin/env bash

set -euo pipefail

curl http://download.redis.io/releases/redis-4.0.9.tar.gz | tar -xzf -
(cd redis-4.0.9 && make && make install)
rm -rf redis-4.0.9
