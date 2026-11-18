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

# ── backup repository config ─────────────────────────────────────────────────
BACKUP_REPO="${BACKUP_REPO:-${XDG_CONFIG_HOME}/backups/emacs.borg}"
BACKUP_PREFIX="${BACKUP_PREFIX:-emacs}"
BACKUP_KEEP="${BACKUP_KEEP:-6}"
# borg takes the repository from the environment. It is unencrypted on purpose
# (local rollback insurance, not an off-site backup), so don't prompt about it.
export BORG_REPO="$BACKUP_REPO"
export BORG_UNKNOWN_UNENCRYPTED_REPO_ACCESS_IS_OK=yes

# ── logging ───────────────────────────────────────────────────────────────────
log() { echo -e "[INFO]  $*"; }
warn() { echo -e "${YELLOW}[WARN]  $*${RESET}" >&2; }
die() {
    echo -e "${RED}[ERROR] $*${RESET}" >&2
    exit 1
}
divider() {
    echo ""
    echo "════════════════════════════════════════════════════════"
    echo "  $*"
    echo "════════════════════════════════════════════════════════"
    echo ""
}

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

require_linux() {
    [[ "$(uname)" == "Linux" ]] || die "Interactive setup is Linux-only."
}

require_borg() {
    command -v borg &>/dev/null || die "borg not found — install borgbackup (see scripts/deps.sh)."
}

setup_banner() {
    # setup_banner "Title" — header shared by the interactive setup scripts
    divider "$1"
    log "User     : $USER"
    log "Home     : $HOME"
    echo ""
    log "Each step will ask for confirmation before running."
    log "You can safely skip steps you have already completed."
    echo ""
}

# ── shared actions ────────────────────────────────────────────────────────────
borg_run() {
    # borg_run <borg args...> — borg exits 1 on warnings (e.g. a file the
    # running daemon changed mid-read); only >= 2 means the command failed.
    local rc=0
    borg "$@" || rc=$?
    ((rc < 2)) || return "$rc"
    ((rc == 0)) || warn "borg $1 finished with warnings (see above)."
}

restart_emacs_daemon() {
    log "Restarting Emacs daemon..."
    systemctl --user restart emacs.service \
        && step_done "Emacs daemon restarted." \
        || {
            step_fail "Failed to restart Emacs daemon."
            return 1
        }
}
