#! /usr/bin/env sh

set -e

exec fastapi run app/main.py --proxy-headers --port "${PORT:-80}" --root-path "${ROOT_PATH}"
