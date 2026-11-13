#!/bin/bash
# Run the Playwright e2e tests on X11 (Xvfb) or Wayland (headless mutter, GNOME's compositor).
# Tests run as an unprivileged user so that Chromium keeps its sandbox enabled.
# Usage: ci/run-e2e-linux.sh x11|wayland [playwright test args]
set -euo pipefail

id e2e >/dev/null 2>&1 || useradd --create-home e2e
# the checkout belongs to root: the tests write launcher logs (job artifacts) and Playwright output elsewhere
mkdir -p launcher-logs
OUTPUT_DIR=$(mktemp -d)
XDG_RUNTIME_DIR=$(mktemp -d)
chown e2e launcher-logs "$OUTPUT_DIR" "$XDG_RUNTIME_DIR"
export XDG_RUNTIME_DIR

DISPLAY_SERVER=$1
shift
case "$DISPLAY_SERVER" in
x11)
  runuser -u e2e -- xvfb-run -a yarn playwright test --output "$OUTPUT_DIR" "$@"
  ;;
wayland)
  # no DISPLAY, so Electron cannot fall back to X11
  runuser -u e2e -- env -u DISPLAY XDG_SESSION_TYPE=wayland WAYLAND_DISPLAY=wayland-0 dbus-run-session -- sh -c '
    set -e
    mutter --headless --wayland --no-x11 --virtual-monitor 1280x1024 &
    timeout 30 sh -c "until [ -S \"$XDG_RUNTIME_DIR/$WAYLAND_DISPLAY\" ]; do sleep 0.2; done"
    yarn playwright test --output "$0" "$@"' "$OUTPUT_DIR" "$@"
  ;;
*)
  echo "Usage: $0 x11|wayland [playwright test args]" >&2
  exit 1
  ;;
esac
