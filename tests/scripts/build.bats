#!/usr/bin/env bats
# Tests for scripts/build.sh

load test_helper

@test "sourcing defines the steps without running the interactive setup" {
    run bash -c 'source "$1/build.sh"; declare -F step_doom step_systemd step_post_install_checks' _ "$SCRIPTS_DIR" </dev/null
    [ "$status" -eq 0 ]
    [[ "$output" != *"Interactive Setup"* ]]
    [ "${#lines[@]}" -eq 3 ]
}

@test "step_doom skips when declined" {
    run bash -c 'source "$1/build.sh"; step_doom' _ "$SCRIPTS_DIR" <<<"n"
    [ "$status" -eq 0 ]
    [[ "$output" == *"doom sync (skipped)"* ]]
    [ ! -s "$STUB_LOG" ]
}

@test "step_doom syncs an existing install" {
    run bash -c 'source "$1/build.sh"; step_doom' _ "$SCRIPTS_DIR" <<<"y"
    [ "$status" -eq 0 ]
    [ "$(cat "$STUB_LOG")" = "doom sync" ]
}
