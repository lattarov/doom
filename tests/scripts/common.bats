#!/usr/bin/env bats
# Tests for scripts/lib/common.sh and scripts/lib/colors.sh

load test_helper

@test "colors are disabled when stdout is not a terminal" {
    run bash -c 'source "$1/lib/common.sh"; printf "[%s]" "$GREEN$RED$YELLOW$RESET"' _ "$SCRIPTS_DIR"
    [ "$status" -eq 0 ]
    [ "$output" = "[]" ]
}

@test "paths derive from XDG_CONFIG_HOME" {
    run bash -c 'source "$1/lib/common.sh"; echo "$EMACS_DIR|$DOOM_BIN|$ARCHIVE_BACKUP_DIR"' _ "$SCRIPTS_DIR"
    [ "$output" = "${EMACS_DIR}|${DOOM_BIN}|${ARCHIVE_BACKUP_DIR}" ]

    unset EMACS_DIR DOOM_BIN ARCHIVE_BACKUP_DIR
    run bash -c 'source "$1/lib/common.sh"; echo "$EMACS_DIR|$DOOM_BIN|$ARCHIVE_BACKUP_DIR"' _ "$SCRIPTS_DIR"
    [ "$output" = "${XDG_CONFIG_HOME}/emacs|${XDG_CONFIG_HOME}/emacs/bin/doom|${XDG_CONFIG_HOME}/backups" ]
}

@test "die prints to stderr and exits 1" {
    run --separate-stderr bash -c 'source "$1/lib/common.sh"; die "boom"; echo unreachable' _ "$SCRIPTS_DIR"
    [ "$status" -eq 1 ]
    [ "$output" = "" ]
    [ "$stderr" = "[ERROR] boom" ]
}

@test "ask: empty answer takes the default" {
    run bash -c 'source "$1/lib/common.sh"; ask "Q?" y' _ "$SCRIPTS_DIR" <<<""
    [ "$status" -eq 0 ]
    run bash -c 'source "$1/lib/common.sh"; ask "Q?" n' _ "$SCRIPTS_DIR" <<<""
    [ "$status" -eq 1 ]
}

@test "ask: explicit answers override the default" {
    run bash -c 'source "$1/lib/common.sh"; ask "Q?" n' _ "$SCRIPTS_DIR" <<<"Y"
    [ "$status" -eq 0 ]
    run bash -c 'source "$1/lib/common.sh"; ask "Q?" y' _ "$SCRIPTS_DIR" <<<"n"
    [ "$status" -eq 1 ]
    run bash -c 'source "$1/lib/common.sh"; ask "Q?" y' _ "$SCRIPTS_DIR" <<<"maybe"
    [ "$status" -eq 1 ]
}

@test "restart_emacs_daemon restarts the user service" {
    run bash -c 'source "$1/lib/common.sh"; restart_emacs_daemon' _ "$SCRIPTS_DIR"
    [ "$status" -eq 0 ]
    [ "$(cat "$STUB_LOG")" = "systemctl --user restart emacs.service" ]
}

@test "restart_emacs_daemon returns 1 when systemctl fails" {
    make_stub "${BATS_TEST_TMPDIR}/stubs/systemctl" 1
    run bash -c 'source "$1/lib/common.sh"; restart_emacs_daemon' _ "$SCRIPTS_DIR"
    [ "$status" -eq 1 ]
}
