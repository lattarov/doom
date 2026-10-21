#!/usr/bin/env bash
# build.sh — Interactive build of Emacs from source (pgtk, native-comp AOT)
#
# Setup order on a fresh machine: deps.sh → build.sh → install.sh
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"
# shellcheck source=lib/common.sh
source "${SCRIPT_DIR}/lib/common.sh"

# Packages only needed to compile Emacs; step_cleanup removes them again.
build_only_packages() {
    local gcc_ver
    gcc_ver=$(gcc -dumpversion | cut -d. -f1)
    BUILD_ONLY_PACKAGES=(
        autoconf
        texinfo
        "libgccjit-${gcc_ver}-dev"
        libgtk-3-dev
        libcairo2-dev
        libharfbuzz-dev
        libotf-dev
        libm17n-dev
        libfreetype-dev
        libjpeg-dev
        libtiff-dev
        libgif-dev
        libpng-dev
        libwebp-dev
        librsvg2-dev
        libxpm-dev
        libmagickwand-dev
        liblcms2-dev
        libgnutls28-dev
        libdbus-1-dev
        libsystemd-dev
        libxml2-dev
        libsqlite3-dev
        libtree-sitter-dev
        libasound2-dev
        libgmp-dev
        libacl1-dev
        libseccomp-dev
        libselinux1-dev
        libgpm-dev
    )
}

is_installed() {
    [[ "$(dpkg-query -W -f='${db:Status-Status}' "$1" 2>/dev/null)" == "installed" ]]
}

# Set by step_build_deps / step_build_emacs. step_cleanup removes only the
# build-only packages this run installed (anything already present, e.g. pulled
# in by deps.sh, stays), and only once Emacs has actually been built.
NEWLY_INSTALLED_PACKAGES=()
EMACS_BUILT=0

# ── step definitions ──────────────────────────────────────────────────────────

step_build_deps() {
    divider "Step 1 — Emacs build dependencies (apt)"
    log "Requires: sudo, apt, gcc"
    echo ""

    ask "Install Emacs build dependencies?" || {
        step_skip "build dependencies"
        return 0
    }

    build_only_packages
    local pkg
    for pkg in "${BUILD_ONLY_PACKAGES[@]}"; do
        is_installed "$pkg" || NEWLY_INSTALLED_PACKAGES+=("$pkg")
    done

    sudo apt update
    # libgccjit0 is the native-comp runtime and stays after step_cleanup.
    sudo apt install -y \
        build-essential \
        curl \
        git \
        gnupg \
        libgccjit0 \
        pkgconf \
        "${BUILD_ONLY_PACKAGES[@]}"

    step_done "Build dependencies installed."
}

step_build_emacs() {
    divider "Step 2 — Build Emacs from source"

    # ── branch/version selection ───────────────────────────────────────────────
    echo "  Which Emacs version do you want to build?"
    echo ""
    echo "    1) Latest stable release (recommended)"

    # fetch latest stable tag from savannah without cloning
    log "Fetching available stable releases..."
    LATEST_STABLE=$(git ls-remote --tags https://git.savannah.gnu.org/git/emacs.git \
        | grep -oP 'emacs-\d+\.\d+(\.\d+)?$' \
        | sort -V \
        | tail -1 \
        || echo "Failed to fetch tags from remote.")

    echo "       → ${LATEST_STABLE}"
    echo ""
    echo "    2) Master branch"
    echo ""
    read -r -p "  Choice [1/2, default 1]: " version_choice
    version_choice="${version_choice:-1}"

    case "$version_choice" in
        2)
            GIT_REF="master"
            GIT_BRANCH_FLAG="--depth=1"
            SOURCE_METHOD="git"
            log "Selected: master (development)"
            ;;
        *)
            GIT_REF="${LATEST_STABLE}"
            TARBALL_NAME="${LATEST_STABLE}.tar.xz"
            TARBALL_URL="https://ftp.gnu.org/gnu/emacs/${TARBALL_NAME}"
            SOURCE_METHOD="tarball"
            log "Selected: ${LATEST_STABLE} (stable)"
            ;;
    esac

    log ""
    log "  Source dir    : ${EMACS_SRC:-$HOME/dev/emacs}"
    if [[ "$SOURCE_METHOD" == "tarball" ]]; then
        log "  Source method : tarball download + GPG verify"
        log "  Release       : ${GIT_REF}"
    else
        log "  Source method : git clone"
        log "  Git ref       : ${GIT_REF}"
    fi
    log "  Build jobs    : $(nproc --ignore=1)"
    log "  Install to    : /usr/local"
    log ""
    warn "This will take several minutes."

    ask "Build and install Emacs?" || {
        step_skip "Emacs build"
        return 0
    }

    EMACS_SRC="${EMACS_SRC:-$HOME/dev/emacs}"
    JOBS=$(nproc --ignore=1)
    SKIP_DOWNLOAD=0

    if [[ "$SOURCE_METHOD" == "git" ]]; then
        if [[ -d "$EMACS_SRC" ]]; then
            log "Source directory exists at ${EMACS_SRC}."
            CURRENT_REF=$(git -C "$EMACS_SRC" describe --tags 2>/dev/null \
                || git -C "$EMACS_SRC" rev-parse --abbrev-ref HEAD 2>/dev/null \
                || echo "non-git checkout")
            log "Currently at: ${CURRENT_REF}"

            if ask "Wipe and reclone? (required when switching stable<->master)" "n"; then
                rm -rf "$EMACS_SRC"
                log "Cloning Emacs ${GIT_REF}..."
                # shellcheck disable=SC2086
                git clone $GIT_BRANCH_FLAG \
                    https://git.savannah.gnu.org/git/emacs.git "$EMACS_SRC"
            else
                log "Pulling latest for ${GIT_REF}..."
                git -C "$EMACS_SRC" fetch --depth=1 origin "${GIT_REF}"
                git -C "$EMACS_SRC" checkout "${GIT_REF}"
                git -C "$EMACS_SRC" reset --hard "origin/${GIT_REF}" 2>/dev/null \
                    || git -C "$EMACS_SRC" reset --hard "${GIT_REF}"
            fi
        else
            log "Cloning Emacs ${GIT_REF}..."
            # shellcheck disable=SC2086
            git clone $GIT_BRANCH_FLAG \
                https://git.savannah.gnu.org/git/emacs.git "$EMACS_SRC"
        fi
    else
        # ── stable path: download the official release tarball and GPG-verify it ──
        if [[ -d "$EMACS_SRC" ]]; then
            log "Source directory exists at ${EMACS_SRC}."
            if ask "Wipe and re-extract? (required when switching stable<->master, or to fetch a newer release)" "n"; then
                rm -rf "$EMACS_SRC"
            else
                log "Reusing existing source at ${EMACS_SRC} — skipping download."
                SKIP_DOWNLOAD=1
            fi
        fi

        if [[ "$SKIP_DOWNLOAD" -eq 0 ]]; then
            if ! command -v gpg &>/dev/null; then
                log "gpg not found — installing gnupg."
                sudo apt install -y gnupg
            fi

            WORKDIR=$(mktemp -d)

            log "Downloading GNU keyring (for signature verification)..."
            curl -L --progress-bar "https://ftp.gnu.org/gnu/gnu-keyring.gpg" \
                -o "${WORKDIR}/gnu-keyring.gpg"

            log "Downloading ${TARBALL_URL}..."
            curl -L --progress-bar "${TARBALL_URL}" -o "${WORKDIR}/${TARBALL_NAME}"
            curl -L --progress-bar "${TARBALL_URL}.sig" -o "${WORKDIR}/${TARBALL_NAME}.sig"

            # Pinned SHA256 checksums for known official releases (defense-in-depth
            # alongside the GPG signature check below; see https://ftp.gnu.org/gnu/emacs/)
            declare -A EMACS_TARBALL_SHA256=(
                ["emacs-31.1.tar.gz"]="3cad7fd1466c0e24867df8d2609da3ac75abc90d7c4c0175e410e9be46d4092a"
                ["emacs-31.1.tar.xz"]="1da5790d9580c81932b5bf700633114468da7b3412d69faa767daebf974f4586"
            )

            if [[ -n "${EMACS_TARBALL_SHA256[$TARBALL_NAME]:-}" ]]; then
                log "Verifying SHA256 checksum..."
                echo "${EMACS_TARBALL_SHA256[$TARBALL_NAME]}  ${WORKDIR}/${TARBALL_NAME}" | sha256sum --check \
                    || die "SHA256 checksum mismatch for ${TARBALL_NAME} — refusing to build untrusted source. (Artifacts left in ${WORKDIR} for inspection.)"
            else
                warn "No pinned SHA256 for ${TARBALL_NAME} — skipping checksum check (GPG signature verification still applies)."
            fi

            log "Verifying GPG signature against GNU keyring..."
            gpg --keyring "${WORKDIR}/gnu-keyring.gpg" --verify \
                "${WORKDIR}/${TARBALL_NAME}.sig" "${WORKDIR}/${TARBALL_NAME}" \
                || die "GPG signature verification FAILED for ${TARBALL_NAME} — refusing to build untrusted source. (Artifacts left in ${WORKDIR} for inspection.)"

            log "Extracting ${TARBALL_NAME}..."
            tar -xf "${WORKDIR}/${TARBALL_NAME}" -C "$WORKDIR"
            mkdir -p "$(dirname "$EMACS_SRC")"
            rm -rf "$EMACS_SRC"
            mv "${WORKDIR}/${LATEST_STABLE}" "$EMACS_SRC"
            rm -rf "$WORKDIR"
        fi
    fi

    cd "$EMACS_SRC"

    if [[ "$SOURCE_METHOD" == "git" ]]; then
        # git checkouts (master) ship no configure script — must generate it
        log "Running autogen..."
        ./autogen.sh
    else
        # official release tarballs already include a pre-generated ./configure
        log "Skipping autogen (release tarball ships a pre-generated ./configure)."
    fi

    log "Configuring..."
    ./configure \
        --with-pgtk \
        --with-native-compilation=aot \
        --with-harfbuzz \
        --with-libotf \
        --with-m17n-flt \
        --with-jpeg \
        --with-tiff \
        --with-gif \
        --with-png \
        --with-webp \
        --with-rsvg \
        --with-imagemagick \
        --with-lcms2 \
        --with-gnutls \
        --with-xml2 \
        --with-dbus \
        --with-libsystemd \
        --with-mailutils \
        --with-tree-sitter \
        --with-sqlite3 \
        --with-modules \
        --with-sound \
        --with-gmp \
        --with-file-notification=inotify \
        --with-wide-int \
        --without-x \
        CFLAGS="-O2 -pipe -mtune=native -march=native -fomit-frame-pointer"

    log "Building with ${JOBS} jobs (NATIVE_FULL_AOT=1)..."
    make -j"${JOBS}" NATIVE_FULL_AOT=1

    log "Installing (requires sudo)..."
    sudo make install
    EMACS_BUILT=1

    step_done "Emacs built and installed ($(emacs --version | head -1))."
}

step_cleanup() {
    divider "Step 3 — Remove build-only packages"

    if [[ "$EMACS_BUILT" -ne 1 ]]; then
        log "Emacs was not built in this run — keeping the build dependencies."
        step_skip "cleanup"
        return 0
    fi
    if [[ ${#NEWLY_INSTALLED_PACKAGES[@]} -eq 0 ]]; then
        log "This run installed no build-only packages — nothing to remove."
        step_skip "cleanup"
        return 0
    fi

    log "Removing the -dev packages this run installed only to compile Emacs:"
    log "  ${NEWLY_INSTALLED_PACKAGES[*]}"
    log "Runtime .so files remain — the installed Emacs is unaffected."
    log "Rerunning this script reinstalls them for the next rebuild."
    echo ""

    # No -y: apt lists everything it will remove and asks for confirmation.
    sudo apt remove --purge "${NEWLY_INSTALLED_PACKAGES[@]}"
    sudo apt autoremove --purge

    step_done "Build packages removed."
}

# ── main ──────────────────────────────────────────────────────────────────────
# When sourced (e.g. by the tests), only define the steps above.
[[ "${BASH_SOURCE[0]}" == "$0" ]] || return 0

require_linux
setup_banner "Doom Emacs — Build Emacs"

step_build_deps
step_build_emacs
step_cleanup

divider "Build done"
log "Next: scripts/install.sh to install Doom Emacs."
