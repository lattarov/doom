#!/usr/bin/env bash
# rollback.sh — Restore the Emacs directory from a snapshot taken by backup.sh
# (or upgrade.sh, which backs up before upgrading).
#
# Usage: rollback.sh [snapshot-name]
#   With no argument, lists available snapshots and prompts for a choice.
#
# The snapshot is extracted to a staging directory first, so a failed extract
# leaves the Emacs directory untouched; it is then synced over the Emacs
# directory, rewriting only the files that differ.
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"
# shellcheck source=lib/common.sh
source "${SCRIPT_DIR}/lib/common.sh"

divider "Rollback Doom Emacs"

require_borg

mapfile -t snapshots < <(borg list --short --sort-by timestamp --glob-archives "${BACKUP_PREFIX}_*" 2>/dev/null | tac)

[[ "${#snapshots[@]}" -gt 0 ]] || die "No snapshots found in ${BORG_REPO}."

snapshot="${1:-}"

if [[ -z "$snapshot" ]]; then
    echo "  Available snapshots (newest first):"
    echo ""
    for i in "${!snapshots[@]}"; do
        printf "    %d) %s\n" "$((i + 1))" "${snapshots[$i]}"
    done
    echo ""
    read -r -p "  Restore which snapshot? [1-${#snapshots[@]}, default 1]: " choice
    choice="${choice:-1}"
    [[ "$choice" =~ ^[0-9]+$ ]] && ((choice >= 1 && choice <= ${#snapshots[@]})) \
        || die "Invalid selection: $choice"
    snapshot="${snapshots[$((choice - 1))]}"
else
    borg info "::${snapshot}" &>/dev/null || die "Snapshot not found in ${BORG_REPO}: ${snapshot}"
fi

log "Selected snapshot: ${snapshot}"

ask "This will overwrite ${EMACS_DIR}. Continue?" "n" || die "Rollback aborted."

## Stage next to the Emacs directory, so it lands on the same filesystem.
emacs_name="$(basename "$EMACS_DIR")"
staging="$(mktemp -d "$(dirname "$EMACS_DIR")/.${emacs_name}-rollback.XXXXXX")"
trap 'rm -rf "$staging"' EXIT

log "Extracting ${snapshot}..."
cd "$staging"
borg_run extract "::${snapshot}" || die "Failed to extract ${snapshot} — ${EMACS_DIR} left untouched."
[[ -d "${staging}/${emacs_name}" ]] || die "Snapshot ${snapshot} holds no ${emacs_name}/ directory."

if [[ -d "$EMACS_DIR" ]]; then
    log "Taking a safety snapshot of the current state before rollback..."
    "${SCRIPT_DIR}/backup.sh" --no-prune \
        || die "Pre-rollback safety snapshot failed — ${EMACS_DIR} left untouched."
fi

log "Stopping Emacs daemon..."
systemctl --user stop emacs.service 2>/dev/null || true

rsync -a --hard-links --delete "${staging}/${emacs_name}/" "${EMACS_DIR}/" \
    || die "Failed to sync ${snapshot} into ${EMACS_DIR} — roll back to the safety snapshot."

step_done "Restored ${EMACS_DIR} from ${snapshot}."

restart_emacs_daemon || die "Restart Emacs command not available!"
