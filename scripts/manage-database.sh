#!/usr/bin/env bash

set -e

declare DELETE=false
declare CREATE=true

# Get the git root and move to the docker directory, so we don't have to provide
# `-f` parameter to docker compose.
source="$(git rev-parse --show-toplevel)"
cd "$source/"

function print_help() {
cat << EOF
Usage: $0 [OPTION]

  -c --create     creates database (default)
  -r --recreate   recreates the databases (equivalent to --create after --delete)
  -d --delete     delete database
  -h --help       prints this message
EOF
}


while [[ $# -gt 0 ]]
do
  key="$1"

  case $key in
    -c|--create)
    DELETE=false
    CREATE=true
    shift
    ;;
    -r|--recreate)
    DELETE=true
    CREATE=true
    shift
    ;;
    -d|--delete)
    DELETE=true
    CREATE=false
    shift
    ;;
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

# Make sure the postgres image is running.
docker compose up -d postgres

RETRIES=10
until docker compose exec postgres pg_isready > /dev/null 2>&1 || [[ ${RETRIES} -eq 0 ]]; do
  echo "Waiting for postgres server, $((RETRIES--)) remaining attempts..."
  sleep 1
done

if [[ ${RETRIES} -eq 0 ]]; then
  echo "faild to start postgres in time"
  exit 1
fi


if [[ ${DELETE} == true ]]; then
  docker compose exec postgres dropdb -h postgres --if-exists anime-rss
fi

if [[ ${CREATE} == true ]]; then
  docker compose exec postgres psql -h postgres -c "SELECT 1 FROM pg_database WHERE datname = 'anime-rss'" | grep -q 1 || docker compose exec postgres createdb -h postgres anime-rss
fi

