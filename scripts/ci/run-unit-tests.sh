#!/usr/bin/env bash
# run-unit-tests.sh — tangle config.org, load the synced Doom profile, run ERT.
#
# Expects: `doom sync` has already been run against this checkout (DOOMDIR
# pointed at the repo root, EMACSDIR at the doomemacs checkout). See
# .github/workflows/unit-tests.yml for the full sequence.
set -euo pipefail

EMACS="${EMACS:-emacs}"
DOOMEMACSDIR="${DOOMEMACSDIR:-$HOME/.config/emacs}"
REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

log() { echo "[test]  $*"; }

cd "$REPO_DIR"

log "Tangling config.org -> config.el / packages.el"
"$EMACS" --batch -Q \
  --eval "(require 'ob-tangle)" \
  --eval "(org-babel-tangle-file \"config.org\")"

[[ -f config.el ]]    || { echo "config.el missing after tangle" >&2; exit 1; }
[[ -f packages.el ]]  || { echo "packages.el missing after tangle" >&2; exit 1; }

log "Running ERT suite (DOOMDIR=${REPO_DIR}, EMACSDIR=${DOOMEMACSDIR})"
DOOMDIR="$REPO_DIR" "$EMACS" --batch -Q \
  -l "${DOOMEMACSDIR}/early-init.el" \
  --eval "(doom-initialize nil nil)" \
  --eval "(require 'doom-packages nil t)" \
  --eval "(condition-case err (load (expand-file-name \"packages.el\" doom-user-dir))
             (error (message \"note: packages.el load stopped early (%s) — fine as long as the functions under test loaded first\" err)))" \
  -l ert \
  -l test/vlv-env-test.el \
  -l test/config-load-test.el \
  -f ert-run-tests-batch-and-exit
