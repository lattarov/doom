#!/usr/bin/env bash
# upgrade.sh — Backup, then upgrade/native-recompile/sync/gc Doom Emacs,
# and restart the Emacs daemon.
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"
# shellcheck source=lib/common.sh
source "${SCRIPT_DIR}/lib/common.sh"

"${SCRIPT_DIR}/backup.sh"

divider "Upgrade Doom Emacs"

JOBS=$(nproc --ignore=1 2>/dev/null || echo 1)

"$DOOM_BIN" upgrade --force --aot --jobs "$JOBS" \
  && step_done "Upgrade complete." || die "Failed to upgrade Doom Emacs!"

"$DOOM_BIN" sync --jobs "$JOBS" \
  && step_done "Sync complete." || die "Failed to sync after upgrade!"

"$DOOM_BIN" gc --force \
  && step_done "Garbage collection complete." || warn "Failed to garbage-collect after upgrade!"

step_done "Upgrade done!"

restart_emacs_daemon || die "Restart Emacs command not available!"
