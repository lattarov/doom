# test_helper.bash — Shared setup for the scripts/*.sh bats tests.

bats_require_minimum_version 1.5.0

# shellcheck disable=SC2034  # used by the .bats files that load this helper
SCRIPTS_DIR="$(cd -- "${BATS_TEST_DIRNAME}/../../scripts" &>/dev/null && pwd)"

setup() {
    export HOME="${BATS_TEST_TMPDIR}/home"
    export XDG_CONFIG_HOME="${HOME}/.config"
    export EMACS_DIR="${XDG_CONFIG_HOME}/emacs"
    export ARCHIVE_BACKUP_DIR="${XDG_CONFIG_HOME}/backups"
    unset ARCHIVE_PREFIX ARCHIVE_FILE_TYPE ARCHIVE_COUNT_MAX DOOM_BIN NO_COLOR

    STUB_LOG="${BATS_TEST_TMPDIR}/stub.log"
    export STUB_LOG
    : >"$STUB_LOG"

    local stub_dir="${BATS_TEST_TMPDIR}/stubs"
    mkdir -p "$stub_dir" "$EMACS_DIR"
    make_stub "${stub_dir}/systemctl"
    make_stub "${stub_dir}/doom"
    export DOOM_BIN="${stub_dir}/doom"
    export PATH="${stub_dir}:${PATH}"

    echo "original" >"${EMACS_DIR}/marker"
}

# make_stub PATH [EXIT_CODE] — a command that logs "name args..." and exits.
make_stub() {
    cat >"$1" <<STUB
#!/usr/bin/env bash
echo "\$(basename "\$0") \$*" >>"\$STUB_LOG"
exit ${2:-0}
STUB
    chmod +x "$1"
}

# make_archive NAME AGE — a backup archive of the current EMACS_DIR, with its
# mtime set AGE in the past (e.g. "3 days ago").
make_archive() {
    mkdir -p "$ARCHIVE_BACKUP_DIR"
    tar --create --gzip --file "${ARCHIVE_BACKUP_DIR}/$1" \
        -C "$(dirname "$EMACS_DIR")" "$(basename "$EMACS_DIR")"
    touch -d "$2" "${ARCHIVE_BACKUP_DIR}/$1"
}

archive_count() {
    find "$ARCHIVE_BACKUP_DIR" -maxdepth 1 -name 'emacs*.tar.gz' | wc -l
}
