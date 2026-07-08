#! /usr/bin/env sh

set -e

exec uvicorn app.main:app \
  --host 0.0.0.0 \
  --port "${PORT:-80}" \
  --workers 2 \
  --loop uvloop \
  --http httptools \
  --limit-concurrency 100 \
  --backlog 1024 \
  --timeout-keep-alive 5 \
  --root-path "${ROOT_PATH}" \
  --no-access-log \
  --log-config app/logging_config.json
