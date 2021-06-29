#!/usr/bin/env bash

set -eax
trap 'kill 0' EXIT

SCRIPT_DIR=$(dirname "$0")
PROJECT_ROOT="${SCRIPT_DIR}/../"
RUNTIME_DIR="${PROJECT_ROOT}/runtime-dir/"


mkdir -p "${RUNTIME_DIR}"

echo "Running nginx"
nginx \
    -g "pid $RUNTIME_DIR/nginx.pid;" \
    -p "$PROJECT_ROOT" \
    -c "$SCRIPT_DIR"/nginx.conf

