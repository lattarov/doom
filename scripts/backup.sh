#!/usr/bin/env bash
# backup.sh — Snapshot the Emacs directory (Doom binaries + all caches) into a
# deduplicating borg repository, pruning snapshots beyond BACKUP_KEEP.
#
# Usage: backup.sh [--no-prune]
#   --no-prune  Keep every snapshot. rollback.sh uses it so its safety
#               snapshot can never prune the snapshot being restored.
#
# Only content that changed since the previous snapshot is stored: git
# refreshes the mtime of its packfiles on every fetch, which makes borg re-read
# them, but identical chunks are never stored twice.

set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"
# shellcheck source=lib/common.sh
source "${SCRIPT_DIR}/lib/common.sh"

case "${1:-}" in
    "") prune=1 ;;
    --no-prune) prune=0 ;;
    *) die "Usage: backup.sh [--no-prune]" ;;
esac

divider "Backup Doom Emacs"

require_borg
[[ -d "$EMACS_DIR" ]] || die "Nothing to back up: ${EMACS_DIR} does not exist."

if [[ ! -d "$BORG_REPO" ]]; then
    log "Creating borg repository ${BORG_REPO}"
    mkdir -p "$(dirname "$BORG_REPO")"
    borg init --encryption=none "$BORG_REPO"
fi

## Get the current Git HEAD SHA-1
git_sha=$(git -C "$EMACS_DIR" rev-parse --short HEAD 2>/dev/null || echo "unknown")

snapshot="${BACKUP_PREFIX}_$(date +%Y-%m-%d_%H-%M-%S)_${git_sha}"

log "Snapshotting ${EMACS_DIR} → ${BORG_REPO}::${snapshot}"
# Store paths relative to the parent ("emacs/..."), as the old tarballs did.
cd "$(dirname "$EMACS_DIR")"
borg_run create --stats --one-file-system --compression auto,zstd,3 \
    "::${snapshot}" "$(basename "$EMACS_DIR")" \
    || die "Failed to snapshot ${EMACS_DIR} — old snapshots left untouched."

step_done "Backup complete: ${snapshot}"

((prune)) || exit 0

## Limit amount of snapshots — only once the new one is safely stored.
borg prune --list --keep-last "$BACKUP_KEEP" --glob-archives "${BACKUP_PREFIX}_*" \
    && borg compact \
    || warn "Failed to prune snapshots beyond the newest ${BACKUP_KEEP}."
