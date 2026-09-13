#!/usr/bin/env bash
# rollback.sh — Restore the Emacs directory from a backup archive created by
# backup.sh (or upgrade.sh, which backs up before upgrading).
#
# Usage: rollback.sh [archive-name-or-path]
#   With no argument, lists available backups and prompts for a choice.
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"
# shellcheck source=lib/common.sh
source "${SCRIPT_DIR}/lib/common.sh"

divider "Rollback Doom Emacs"

mapfile -t archives < <(ls -1t "${ARCHIVE_BACKUP_DIR}/${ARCHIVE_PREFIX}"*".${ARCHIVE_FILE_TYPE}" 2>/dev/null || true)

[[ "${#archives[@]}" -gt 0 ]] || die "No backup archives found in ${ARCHIVE_BACKUP_DIR}."

archive_path="${1:-}"

if [[ -z "$archive_path" ]]; then
  echo "  Available backups (newest first):"
  echo ""
  for i in "${!archives[@]}"; do
    printf "    %d) %s\n" "$((i + 1))" "$(basename "${archives[$i]}")"
  done
  echo ""
  read -r -p "  Restore which backup? [1-${#archives[@]}, default 1]: " choice
  choice="${choice:-1}"
  [[ "$choice" =~ ^[0-9]+$ ]] && (( choice >= 1 && choice <= ${#archives[@]} )) \
    || die "Invalid selection: $choice"
  archive_path="${archives[$((choice - 1))]}"
elif [[ ! -f "$archive_path" ]]; then
  # allow passing just a filename, resolved against the backup dir
  candidate="${ARCHIVE_BACKUP_DIR}/$(basename "$archive_path")"
  [[ -f "$candidate" ]] || die "Backup archive not found: $archive_path"
  archive_path="$candidate"
fi

log "Selected backup: $(basename "$archive_path")"

ask "This will overwrite ${EMACS_DIR}. Continue?" "n" || die "Rollback aborted."

log "Taking a safety snapshot of the current state before rollback..."
"${SCRIPT_DIR}/backup.sh" || warn "Pre-rollback safety backup failed — continuing anyway."

log "Stopping Emacs daemon..."
systemctl --user stop emacs.service 2>/dev/null || true

rm -rf "$EMACS_DIR"
tar --extract --gzip --file "$archive_path" -C "$(dirname "$EMACS_DIR")"

step_done "Restored ${EMACS_DIR} from $(basename "$archive_path")."

restart_emacs_daemon || die "Restart Emacs command not available!"
