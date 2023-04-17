#!/usr/bin/env bash

set -e

DATABASE_CONNECTION_STRING="host=postgres dbname=anime-rss user=postgres" \
OTEL_LOG_ENDPOINT="http://opentelemetry-collector:4218/v1/logs" \
OTEL_TRACE_ENDPOINT="http://opentelemetry-collector:4218/v1/traces" \
cabal run anime-rss
