#!/usr/bin/env bats
# Tests for scripts/backup.sh

load test_helper

@test "archives EMACS_DIR into the backup dir" {
    run "${SCRIPTS_DIR}/backup.sh"
    [ "$status" -eq 0 ]
    [ "$(archive_count)" -eq 1 ]

    local archive
    archive="$(find "$ARCHIVE_BACKUP_DIR" -name 'emacs_*.tar.gz')"
    [[ "$(basename "$archive")" =~ ^emacs_[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}-[0-9]{2}-[0-9]{2}_unknown\.tar\.gz$ ]]
    [ "$(tar --extract --gzip --to-stdout --file "$archive" emacs/marker)" = "original" ]
}

@test "names the archive after the Doom git HEAD" {
    git -C "$EMACS_DIR" init -q
    git -C "$EMACS_DIR" -c user.name=t -c user.email=t@t commit -q --allow-empty -m init
    local sha
    sha="$(git -C "$EMACS_DIR" rev-parse --short HEAD)"

    run "${SCRIPTS_DIR}/backup.sh"
    [ "$status" -eq 0 ]
    [ -n "$(find "$ARCHIVE_BACKUP_DIR" -name "emacs_*_${sha}.tar.gz")" ]
}

@test "keeps only the ARCHIVE_COUNT_MAX newest archives" {
    make_archive emacs_old1.tar.gz "4 days ago"
    make_archive emacs_old2.tar.gz "3 days ago"
    make_archive emacs_old3.tar.gz "2 days ago"

    run "${SCRIPTS_DIR}/backup.sh"
    [ "$status" -eq 0 ]
    [ "$(archive_count)" -eq 3 ]
    [ ! -e "${ARCHIVE_BACKUP_DIR}/emacs_old1.tar.gz" ]
    [ -e "${ARCHIVE_BACKUP_DIR}/emacs_old3.tar.gz" ]
    [[ "$output" == *"Removed oldest archive: emacs_old1.tar.gz"* ]]
}

@test "leaves unrelated files in the backup dir alone" {
    make_archive emacs_old1.tar.gz "2 days ago"
    touch "${ARCHIVE_BACKUP_DIR}/notes.txt"
    ARCHIVE_COUNT_MAX=1 run "${SCRIPTS_DIR}/backup.sh"
    [ "$status" -eq 0 ]
    [ "$(archive_count)" -eq 1 ]
    [ -e "${ARCHIVE_BACKUP_DIR}/notes.txt" ]
}

@test "fails without rotating when EMACS_DIR is missing" {
    make_archive emacs_old1.tar.gz "2 days ago"
    rm -rf "$EMACS_DIR"
    ARCHIVE_COUNT_MAX=1 run "${SCRIPTS_DIR}/backup.sh"
    [ "$status" -eq 1 ]
    [ -e "${ARCHIVE_BACKUP_DIR}/emacs_old1.tar.gz" ]
    [ "$(archive_count)" -eq 1 ]
}
