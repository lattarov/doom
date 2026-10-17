#!/usr/bin/env bats
# Tests for scripts/rollback.sh

load test_helper

@test "fails when there are no backups" {
    run "${SCRIPTS_DIR}/rollback.sh" <<<"y"
    [ "$status" -eq 1 ]
    [[ "$output" == *"No backup archives found"* ]]
}

@test "restores the archive named on the command line" {
    make_archive emacs_good.tar.gz "1 day ago"
    echo "broken" >"${EMACS_DIR}/marker"
    echo "junk" >"${EMACS_DIR}/new-file"

    run "${SCRIPTS_DIR}/rollback.sh" emacs_good.tar.gz <<<"y"
    [ "$status" -eq 0 ]
    [ "$(cat "${EMACS_DIR}/marker")" = "original" ]
    [ ! -e "${EMACS_DIR}/new-file" ]
}

@test "takes a safety backup and restarts the daemon" {
    make_archive emacs_good.tar.gz "1 day ago"
    run "${SCRIPTS_DIR}/rollback.sh" emacs_good.tar.gz <<<"y"
    [ "$status" -eq 0 ]
    [ "$(archive_count)" -eq 2 ]
    [ "$(cat "$STUB_LOG")" = $'systemctl --user stop emacs.service\nsystemctl --user restart emacs.service' ]
}

@test "aborts without touching EMACS_DIR when not confirmed" {
    make_archive emacs_good.tar.gz "1 day ago"
    echo "current" >"${EMACS_DIR}/marker"

    run "${SCRIPTS_DIR}/rollback.sh" emacs_good.tar.gz <<<""
    [ "$status" -eq 1 ]
    [[ "$output" == *"Rollback aborted"* ]]
    [ "$(cat "${EMACS_DIR}/marker")" = "current" ]
    [ ! -s "$STUB_LOG" ]
}

@test "rejects an unknown archive name" {
    make_archive emacs_good.tar.gz "1 day ago"
    run "${SCRIPTS_DIR}/rollback.sh" emacs_missing.tar.gz <<<"y"
    [ "$status" -eq 1 ]
    [[ "$output" == *"Backup archive not found"* ]]
}

@test "interactive menu defaults to the newest backup" {
    echo "older" >"${EMACS_DIR}/marker"
    make_archive emacs_older.tar.gz "2 days ago"
    echo "newer" >"${EMACS_DIR}/marker"
    make_archive emacs_newer.tar.gz "1 day ago"
    echo "current" >"${EMACS_DIR}/marker"

    run "${SCRIPTS_DIR}/rollback.sh" <<<$'\ny'
    [ "$status" -eq 0 ]
    [ "$(cat "${EMACS_DIR}/marker")" = "newer" ]
}

@test "interactive menu restores the chosen backup" {
    echo "older" >"${EMACS_DIR}/marker"
    make_archive emacs_older.tar.gz "2 days ago"
    echo "newer" >"${EMACS_DIR}/marker"
    make_archive emacs_newer.tar.gz "1 day ago"

    run "${SCRIPTS_DIR}/rollback.sh" <<<$'2\ny'
    [ "$status" -eq 0 ]
    [ "$(cat "${EMACS_DIR}/marker")" = "older" ]
}

@test "interactive menu rejects an out-of-range choice" {
    make_archive emacs_good.tar.gz "1 day ago"
    run "${SCRIPTS_DIR}/rollback.sh" <<<$'5\ny'
    [ "$status" -eq 1 ]
    [[ "$output" == *"Invalid selection: 5"* ]]
}
