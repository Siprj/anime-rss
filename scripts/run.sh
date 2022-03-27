#!/usr/bin/env bash

set -eax
trap 'kill 0' EXIT

SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"
PROJECT_ROOT="${SCRIPT_DIR}/../"
RUNTIME_DIR="${PROJECT_ROOT}/runtime-dir/"

cabal run anime-rss-server &
${SCRIPT_DIR}/run-nginx.sh
