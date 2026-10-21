#!/usr/bin/env bash
# install.sh — Interactive Doom Emacs install (Doom, emacs.service, native modules)
#
# Setup order on a fresh machine: deps.sh → build.sh → install.sh
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"
# shellcheck source=lib/common.sh
source "${SCRIPT_DIR}/lib/common.sh"

# ── step definitions ──────────────────────────────────────────────────────────

step_doom() {
    divider "Step 1 — Doom Emacs"

    if [[ -d "$EMACS_DIR" ]]; then
        log "Doom already present at ${EMACS_DIR}."
        ask "Run doom sync?" || {
            step_skip "doom sync"
            return 0
        }
        "$DOOM_BIN" sync
        step_done "doom sync complete."
    else
        ask "Install Doom Emacs?" || {
            step_skip "Doom install"
            return 0
        }
        git clone --depth=1 https://github.com/doomemacs/doomemacs "$EMACS_DIR"
        "$DOOM_BIN" install
        step_done "Doom Emacs installed."
    fi
}

step_systemd() {
    divider "Step 2 — Emacs systemd daemon"
    log "Installs and enables an Emacs user service that starts on login."

    SYSTEMD_UNIT="${HOME}/.config/systemd/user/emacs.service"

    if [[ -f "$SYSTEMD_UNIT" ]]; then
        log "Unit file already exists at ${SYSTEMD_UNIT}."
        ask "Overwrite and restart?" "n" || {
            step_skip "systemd unit"
            return 0
        }
        systemctl --user stop emacs.service 2>/dev/null || true
    else
        ask "Install Emacs systemd daemon?" || {
            step_skip "systemd daemon"
            return 0
        }
    fi

    mkdir -p "$(dirname "$SYSTEMD_UNIT")"
    cat >"$SYSTEMD_UNIT" <<'EOF'
[Unit]
Description=Emacs text editor
Documentation=info:emacs man:emacs(1) https://gnu.org/software/emacs/

[Service]
Type=notify
Environment=GDK_BACKEND=wayland
Environment=EMACS_PGTK_FORCE_WAYLAND=1
Environment=SSH_AUTH_SOCK=%t/keyring/ssh
ExecStart=/usr/local/bin/emacs --fg-daemon
ExecStop=/usr/local/bin/emacsclient --eval "(kill-emacs)"
Restart=on-failure

[Install]
WantedBy=default.target
EOF

    systemctl --user daemon-reload
    systemctl --user enable --now emacs.service
    systemctl --user is-active --quiet emacs.service \
        || die "emacs.service failed to start — see: journalctl --user -u emacs.service"
    step_done "Emacs daemon enabled and started."
}

step_first_run_setup() {
    divider "Step 3 — First-run installs (vterm, ghostel, whisper.cpp)"
    log "Triggers the same native-module/model downloads Emacs would otherwise"

    ask "Run first-run installs now?" || {
        step_skip "first-run installs"
        return 0
    }

    local started_temp_daemon=0
    if ! emacsclient --eval 't' &>/dev/null; then
        log "No Emacs daemon found — starting a temporary one."
        emacs --daemon
        started_temp_daemon=1
    fi

    log "Compiling vterm native module..."
    emacsclient --eval '(vterm-module-compile)' &>/dev/null \
        && step_done "vterm module compiled" || step_fail "vterm module compile failed"

    log "Downloading ghostel native module..."
    emacsclient --eval \
        '(progn (require (quote ghostel))
            (let ((ghostel-module-auto-install (quote download)))
              (ghostel--ensure-module (ghostel--module-directory))))' &>/dev/null \
        && step_done "ghostel module installed" || step_fail "ghostel module install failed"

    log "Installing whisper.cpp and the default speech model..."
    WHISPER_DIR=$(emacsclient --eval \
        '(progn (require (quote whisper)) (expand-file-name whisper--install-path))' \
        2>/dev/null | tr -d '"')
    WHISPER_MODEL=$(emacsclient --eval 'whisper-model' 2>/dev/null | tr -d '"')
    WHISPER_MODEL="${WHISPER_MODEL:-base}"
    if [[ -n "$WHISPER_DIR" ]]; then
        mkdir -p "$(dirname "$WHISPER_DIR")"
        if [[ ! -d "$WHISPER_DIR" ]]; then
            git clone https://github.com/ggerganov/whisper.cpp "$WHISPER_DIR"
        fi
        (cd "$WHISPER_DIR" && CLICOLOR=0 cmake -B build && CLICOLOR=0 cmake --build build -j --config Release) \
            && (cd "$WHISPER_DIR" && "models/download-ggml-model.sh" "$WHISPER_MODEL") \
            && step_done "whisper.cpp + '${WHISPER_MODEL}' model ready" \
            || step_fail "whisper.cpp/model install failed"
    else
        step_fail "could not resolve whisper install directory — is the whisper package loaded?"
    fi

    if [[ "$started_temp_daemon" -eq 1 ]]; then
        log "Stopping temporary Emacs daemon."
        emacsclient --eval '(kill-emacs)' &>/dev/null || true
    fi

    step_done "First-run installs complete."
}

step_post_install_checks() {
    divider "Step 4 — Post-install checks"

    local started_temp_daemon=0
    if ! emacsclient --eval 't' &>/dev/null; then
        emacs --daemon
        started_temp_daemon=1
    fi

    local native_comp

    native_comp=$(emacsclient --eval '(if (and (fboundp (quote native-comp-available-p)) (native-comp-available-p)) "t" "nil")' 2>/dev/null | tr -d '"')
    [[ "$native_comp" == "t" ]] \
        && step_done "Native compilation" \
        || step_fail "Native compilation unavailable"

    [[ "$started_temp_daemon" -eq 1 ]] && emacsclient --eval '(kill-emacs)' &>/dev/null || true
}

# ── summary ───────────────────────────────────────────────────────────────────
print_summary() {
    divider "Setup complete"
    log "vterm, ghostel, whisper.cpp installed in Step 3; checked in Step 4."
}

# ── main ──────────────────────────────────────────────────────────────────────
# When sourced (e.g. by the nightly CI job), only define the steps above.
[[ "${BASH_SOURCE[0]}" == "$0" ]] || return 0

require_linux
setup_banner "Doom Emacs — Install"

step_doom
step_systemd
step_first_run_setup
step_post_install_checks
print_summary
