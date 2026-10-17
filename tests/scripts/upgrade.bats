#!/usr/bin/env bats
# Tests for scripts/upgrade.sh

load test_helper

@test "backs up, upgrades, syncs, gcs, then restarts the daemon" {
    run "${SCRIPTS_DIR}/upgrade.sh"
    [ "$status" -eq 0 ]
    [ "$(archive_count)" -eq 1 ]

    local jobs
    jobs="$(nproc --ignore=1)"
    [ "$(cat "$STUB_LOG")" = "doom upgrade --force --aot --jobs ${jobs}
doom sync --jobs ${jobs}
doom gc --force
systemctl --user restart emacs.service" ]
}

@test "stops before syncing when the upgrade fails" {
    make_stub "$DOOM_BIN" 1
    run "${SCRIPTS_DIR}/upgrade.sh"
    [ "$status" -eq 1 ]
    [[ "$output" == *"Failed to upgrade Doom Emacs"* ]]
    [ "$(cat "$STUB_LOG")" = "doom upgrade --force --aot --jobs $(nproc --ignore=1)" ]
}

@test "does not upgrade when the backup fails" {
    rm -rf "$EMACS_DIR"
    run "${SCRIPTS_DIR}/upgrade.sh"
    [ "$status" -eq 1 ]
    [ ! -s "$STUB_LOG" ]
}
