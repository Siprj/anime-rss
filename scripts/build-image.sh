#!/bin/sh -e

source="$(git rev-parse --show-toplevel)"
cd "$source/"

docker compose up -d
docker compose exec anime-rss-devel bash -c "./scripts/build-dists.sh"
DOCKER_BUILDKIT="1" docker build --target anime-rss-service -f docker/Dockerfile -t anime-rss-service:v0.0.4 .
DOCKER_BUILDKIT="1" docker build --target anime-rss-nginx -f docker/Dockerfile -t anime-rss-nginx:v0.0.4 .

docker save -o anime-rss-service.tar anime-rss-service:v0.0.4
docker save -o anime-rss-nginx.tar anime-rss-nginx:v0.0.4
