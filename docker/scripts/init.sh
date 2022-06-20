#!/usr/bin/env bash

set -ex

dcversion=$(docker compose version)
if ! grep -q -E "version v?2" <<< "$dcversion"; then
  echo "To use this setup, you need docker compose v2." >&2
  echo "Your version: $dcversion" >&2
  exit 1
fi

# Make sure the docker kit is on to allow faster builds, and change working
# directory to the docker directory so there is no need to provide `-f`
# parameter to `docker compose`.
export DOCKER_BUILDKIT=1
source="$(git rev-parse --show-toplevel)"
cd "$source/docker/"

function print_help() {
cat << EOF
Usage: $0 [OPTION]

  Initialize the docker environment for easy development.

  -h --help              prints this message
EOF
}


while [[ $# -gt 0 ]]
do
  key="$1"

  case $key in
    -h|--help)
    print_help
    exit 0
    shift
    ;;
    *)
    echo "ERROR: unknown argument [${key}]"
    print_help
    exit 1
    shift
    ;;
  esac
done

# Generate the necessary environment variables for docker compose.
cat <<EOF > "${source}/docker/.env"
UID=$UID
SOURCE_DIRECTORY=$source
COMPOSE_PROJECT_NAME=anime-rss
EOF


# Initialize the databases.
./scripts/manage-database.sh --recreate

docker compose build devel
docker compose up -d devel
