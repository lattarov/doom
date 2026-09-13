#!/usr/bin/env bash
# common.sh — Shared configuration and helpers for scripts/*.sh
#
# Source this from a script with:
#   SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"
#   source "${SCRIPT_DIR}/lib/common.sh"

SCRIPT_LIB_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"
# shellcheck source=colors.sh
source "${SCRIPT_LIB_DIR}/colors.sh"

# ── paths ─────────────────────────────────────────────────────────────────────
XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
EMACS_DIR="${EMACS_DIR:-${XDG_CONFIG_HOME}/emacs}"
DOOM_BIN="${DOOM_BIN:-${EMACS_DIR}/bin/doom}"

# ── backup archive config ────────────────────────────────────────────────────
ARCHIVE_PREFIX="${ARCHIVE_PREFIX:-emacs}"
ARCHIVE_FILE_TYPE="${ARCHIVE_FILE_TYPE:-tar.gz}"
ARCHIVE_COUNT_MAX="${ARCHIVE_COUNT_MAX:-3}"
ARCHIVE_BACKUP_DIR="${ARCHIVE_BACKUP_DIR:-${XDG_CONFIG_HOME}/backups}"

# ── logging ───────────────────────────────────────────────────────────────────
log()     { echo -e "[INFO]  $*"; }
warn()    { echo -e "${YELLOW}[WARN]  $*${RESET}" >&2; }
die()     { echo -e "${RED}[ERROR] $*${RESET}" >&2; exit 1; }
divider() { echo ""; echo "════════════════════════════════════════════════════════"; echo "  $*"; echo "════════════════════════════════════════════════════════"; echo ""; }

step_done() { echo -e "  ${GREEN}✔${RESET}  $*"; }
step_skip() { echo "  ─  $* (skipped)"; }
step_fail() { echo -e "  ${RED}✘${RESET}  $*" >&2; }

ask() {
  # ask "Question?" [default: y|n] => returns 0 for yes, 1 for no
  local prompt="$1"
  local default="${2:-y}"
  local yn
  if [[ "$default" == "y" ]]; then
    read -r -p "  $prompt [Y/n] " yn
    yn="${yn:-y}"
  else
    read -r -p "  $prompt [y/N] " yn
    yn="${yn:-n}"
  fi
  [[ "$yn" =~ ^[Yy]$ ]]
}

# ── shared actions ────────────────────────────────────────────────────────────
restart_emacs_daemon() {
  log "Restarting Emacs daemon..."
  systemctl --user restart emacs.service \
    && step_done "Emacs daemon restarted." \
    || { step_fail "Failed to restart Emacs daemon."; return 1; }
}
