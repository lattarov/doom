#!/usr/bin/env bash
# backup.sh — Archive the Emacs directory (Doom binaries + all caches),
# rotating out old archives beyond ARCHIVE_COUNT_MAX.
#
# A temporary, brute-force, simplest way to have a full backup of the $EMACS_DIR.


set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"
# shellcheck source=lib/common.sh
source "${SCRIPT_DIR}/lib/common.sh"

divider "Backup Doom Emacs"

mkdir -p "$ARCHIVE_BACKUP_DIR"

mapfile -t archives < <(ls -1t "${ARCHIVE_BACKUP_DIR}/${ARCHIVE_PREFIX}"*".${ARCHIVE_FILE_TYPE}" 2>/dev/null || true)

## Limit amount of archives — remove the oldest before adding a new one.
while [[ "${#archives[@]}" -ge "$ARCHIVE_COUNT_MAX" ]]; do
  oldest="${archives[-1]}"
  rm -f "$oldest" && warn "Removed oldest archive: $(basename "$oldest")"
  unset 'archives[-1]'
  archives=("${archives[@]}")
done

## Get the current Git HEAD SHA-1
git_sha=$(git -C "$EMACS_DIR" rev-parse --short HEAD 2>/dev/null || echo "unknown")

archive_name="${ARCHIVE_PREFIX}_$(date +%Y-%m-%d_%H-%M-%S)_${git_sha}.${ARCHIVE_FILE_TYPE}"
archive_path="${ARCHIVE_BACKUP_DIR}/${archive_name}"

log "Archiving ${EMACS_DIR} → ${archive_path}"
tar --create --gzip --file "$archive_path" -C "$(dirname "$EMACS_DIR")" "$(basename "$EMACS_DIR")"

step_done "Backup complete: ${archive_name}"
