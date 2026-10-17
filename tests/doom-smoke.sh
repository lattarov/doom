#!/usr/bin/env bash
# doom-smoke.sh — Fail if Doom signalled an error while starting up. Warnings
# are printed but do not fail the check.
#
# Usage: doom-smoke.sh [SERVER]
#   With SERVER, checks that already-running daemon (e.g. "server", started by
#   emacs.service), waiting up to 60s for its socket. Without, boots a
#   throwaway daemon under its own server name, so it never touches a running
#   one, and stops it without running `kill-emacs-hook' so no
#   session/recentf/savehist state is written back.
set -euo pipefail

EMACS="${EMACS:-emacs}"
EMACSCLIENT="${EMACSCLIENT:-emacsclient}"
SERVER="${1:-}"

stop_daemon() {
    "$EMACSCLIENT" -s "$SERVER" --eval '(let (kill-emacs-hook) (kill-emacs 0))' &>/dev/null || true
}

if [[ -z "$SERVER" ]]; then
    SERVER="doom-smoke-$$"
    trap stop_daemon EXIT
    "$EMACS" --daemon="$SERVER"
else
    for _ in $(seq 60); do
        "$EMACSCLIENT" -s "$SERVER" --eval t &>/dev/null && break
        sleep 1
    done
fi

result="$("$EMACSCLIENT" -s "$SERVER" --eval '
(format "%S"
        (list :error init-file-had-error
              :init-time (bound-and-true-p doom-init-time)
              :warnings (when (get-buffer "*Warnings*")
                          (with-current-buffer "*Warnings*"
                            (buffer-substring-no-properties (point-min) (point-max))))))')"

# emacsclient prints the string readably; let Emacs decode it.
decoded="$("$EMACS" --batch -Q --eval "(princ (read $result))")"
echo "$decoded"

[[ "$decoded" == "(:error nil "* ]] || {
    echo "doom-smoke: Doom reported an error during startup" >&2
    exit 1
}
[[ "$decoded" != *":init-time nil "* ]] || {
    echo "doom-smoke: Doom never finished initialising (doom-init-time is nil)" >&2
    exit 1
}
echo "doom-smoke: Doom started cleanly"
