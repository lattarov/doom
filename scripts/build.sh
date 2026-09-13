#!/usr/bin/env bash
# build.sh — Interactive Doom Emacs setup
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"
# shellcheck source=lib/common.sh
source "${SCRIPT_DIR}/lib/common.sh"

# ── platform detection ────────────────────────────────────────────────────────
if [[ "${OS:-}" == "Windows_NT" ]]; then
  PLATFORM="windows"
elif [[ "$(uname)" == "Linux" ]]; then
  PLATFORM="linux"
else
  die "Unsupported platform."
fi

# ── step definitions ──────────────────────────────────────────────────────────

step_apt_deps() {
  divider "Step 1 — System packages (apt)"
  log "This installs all build and runtime dependencies."
  log "Requires: sudo, apt, gcc"
  echo ""

  ask "Install system packages?" || { step_skip "apt packages"; return 0; }

  GCC_VER=$(gcc -dumpversion | cut -d. -f1)
  sudo apt update
  sudo apt install -y \
    "libgccjit-${GCC_VER}-dev" \
    autoconf \
    build-essential \
    clangd \
    cmake \
    default-jre \
    fd-find \
    ffmpeg \
    ffmpegthumbnailer \
    gdb \
    git \
    graphviz \
    imagemagick \
    isync \
    libacl1-dev \
    libasound2-dev \
    libcairo2-dev \
    libdbus-1-dev \
    libenchant-2-dev \
    libfreetype-dev \
    libgccjit0 \
    libgif-dev \
    libgmp-dev \
    libgnutls28-dev \
    libgpm-dev \
    libgtk-3-dev \
    libharfbuzz-dev \
    libjpeg-dev \
    liblcms2-dev \
    libm17n-dev \
    libmagickwand-dev \
    libotf-dev \
    libpng-dev \
    libpoppler-glib-dev \
    librsvg2-dev \
    libseccomp-dev \
    libselinux1-dev \
    libsqlite3-dev \
    libsystemd-dev \
    libtiff-dev \
    libtool-bin \
    libtree-sitter-dev \
    libvterm-dev \
    libwebp-dev \
    libxml2-dev \
    libxpm-dev \
    maildir-utils \
    maim \
    mediainfo \
    npm \
    pandoc \
    pkgconf \
    plantuml \
    poppler-utils \
    python3-pip \
    ripgrep \
    sqlite3 \
    texinfo \
    texlive-fonts-extra \
    texlive-fonts-recommended \
    texlive-latex-base \
    texlive-latex-extra \
    wl-clipboard

  log "Installing Cascadia Code (doom-font)..."
  sudo apt install -y fonts-cascadia-code && fc-cache -f \
    || warn "fonts-cascadia-code not available via apt — install Cascadia Code manually from https://github.com/microsoft/cascadia-code/releases"

  step_done "System packages installed."
}

step_languagetool() {
  divider "Step 2 — LanguageTool (grammar checker)"
  log "LanguageTool is not in apt — downloaded directly from languagetool.org."

  LT_VERSION="6.4"
  LT_DIR="${HOME}/.local/share/languagetool"
  LT_ZIP="LanguageTool-${LT_VERSION}.zip"
  LT_URL="https://languagetool.org/download/${LT_ZIP}"
  LT_SHA256="7f5be0e0b5f1e0745aecf15dd4ee58c3f22b9bd62044af9bb5ac59f5ba90cf26"

  if [[ -d "${LT_DIR}/LanguageTool-${LT_VERSION}" ]]; then
    log "LanguageTool ${LT_VERSION} already present at ${LT_DIR}."
    ask "Reinstall?" "n" || { step_skip "LanguageTool"; return 0; }
    rm -rf "${LT_DIR:?}/LanguageTool-${LT_VERSION}"
  else
    ask "Install LanguageTool ${LT_VERSION}?" || { step_skip "LanguageTool"; return 0; }
  fi

  log "Downloading ${LT_URL}..."
  curl -L --progress-bar "${LT_URL}" -o "/tmp/${LT_ZIP}"

  log "Verifying checksum..."
  echo "${LT_SHA256}  /tmp/${LT_ZIP}" | sha256sum --check \
    || die "Checksum mismatch — aborting LanguageTool install."

  mkdir -p "${LT_DIR}"
  unzip -q "/tmp/${LT_ZIP}" -d "${LT_DIR}"
  rm "/tmp/${LT_ZIP}"

  LT_JAR=$(find "${LT_DIR}" -name "languagetool-commandline.jar" | head -1)
  step_done "LanguageTool installed."
  log "Add to config.el:"
  log "  (setq langtool-language-tool-jar \"${LT_JAR}\")"
}

step_rust() {
  divider "Step 3 — Rust toolchain"
  log "Installs rustup, rust-analyzer, and emacs-lsp-booster."

  if command -v rustup &>/dev/null; then
    log "rustup already installed ($(rustup --version 2>/dev/null | head -1))."
    ask "Update and reinstall components anyway?" "n" || { step_skip "Rust"; return 0; }
  else
    ask "Install Rust toolchain?" || { step_skip "Rust"; return 0; }
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
    source "${HOME}/.cargo/env"
  fi

  rustup update
  rustup component add rust-analyzer

  if cargo install --list | grep -q "emacs-lsp-booster"; then
    log "emacs-lsp-booster already installed."
    ask "Reinstall?" "n" && cargo install --force emacs-lsp-booster \
      || step_skip "emacs-lsp-booster"
  else
    cargo install emacs-lsp-booster
  fi

  step_done "Rust toolchain ready."
}

step_npm() {
  divider "Step 4 — npm global packages (pyright)"
  log "Configures npm to use ~/.local to avoid requiring sudo."

  ask "Install npm packages?" || { step_skip "npm packages"; return 0; }

  NPM_PREFIX="${HOME}/.local"
  npm config set prefix "${NPM_PREFIX}"
  export PATH="${NPM_PREFIX}/bin:${PATH}"

  npm install -g pyright

  step_done "npm packages installed."
  log "Make sure ~/.local/bin is in your PATH:"
  log "  export PATH=\"\$HOME/.local/bin:\$PATH\""
}

step_build_emacs() {
  divider "Step 5 — Build Emacs from source"

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

  ask "Build and install Emacs?" || { step_skip "Emacs build"; return 0; }

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

  step_done "Emacs built and installed ($(emacs --version | head -1))."
}

step_doom() {
  divider "Step 6 — Doom Emacs"

  if [[ -d "$EMACS_DIR" ]]; then
    log "Doom already present at ${EMACS_DIR}."
    ask "Run doom sync?" || { step_skip "doom sync"; return 0; }
    "$DOOM_BIN" sync
    step_done "doom sync complete."
  else
    ask "Install Doom Emacs?" || { step_skip "Doom install"; return 0; }
    git clone --depth=1 https://github.com/doomemacs/doomemacs "$EMACS_DIR"
    "$DOOM_BIN" install
    step_done "Doom Emacs installed."
  fi
}

step_systemd() {
  divider "Step 7 — Emacs systemd daemon"
  log "Installs and enables an Emacs user service that starts on login."

  SYSTEMD_UNIT="${HOME}/.config/systemd/user/emacs.service"

  if [[ -f "$SYSTEMD_UNIT" ]]; then
    log "Unit file already exists at ${SYSTEMD_UNIT}."
    ask "Overwrite and restart?" "n" || { step_skip "systemd unit"; return 0; }
    systemctl --user stop emacs.service 2>/dev/null || true
  else
    ask "Install Emacs systemd daemon?" || { step_skip "systemd daemon"; return 0; }
  fi

  mkdir -p "$(dirname "$SYSTEMD_UNIT")"
  cat > "$SYSTEMD_UNIT" <<'EOF'
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
  step_done "Emacs daemon enabled and started."
}

step_first_run_setup() {
  divider "Step 8 — First-run installs (vterm, ghostel, whisper.cpp)"
  log "Triggers the same native-module/model downloads Emacs would otherwise"

  ask "Run first-run installs now?" || { step_skip "first-run installs"; return 0; }

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
    ( cd "$WHISPER_DIR" && CLICOLOR=0 cmake -B build && CLICOLOR=0 cmake --build build -j --config Release ) \
      && ( cd "$WHISPER_DIR" && "models/download-ggml-model.sh" "$WHISPER_MODEL" ) \
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

step_cleanup() {
  divider "Step 9 — Remove build-only packages"
  log "The following -dev packages are only needed at compile time."
  log "Runtime .so files remain — the running Emacs binary is unaffected."
  warn "Do NOT remove these if you plan to rebuild Emacs or if vterm"
  warn "has not yet compiled its native module (run M-x +vterm/here first)."
  echo ""

  ask "Remove build-only dev packages?" "n" || { step_skip "cleanup"; return 0; }

  GCC_VER=$(gcc -dumpversion | cut -d. -f1)
  sudo apt remove --purge \
    build-essential \
    autoconf \
    texinfo \
    "libgccjit-${GCC_VER}-dev" \
    libgtk-3-dev \
    libcairo2-dev \
    libharfbuzz-dev \
    libotf-dev \
    libm17n-dev \
    libfreetype-dev \
    libjpeg-dev \
    libtiff-dev \
    libgif-dev \
    libpng-dev \
    libwebp-dev \
    librsvg2-dev \
    libxpm-dev \
    libmagickwand-dev \
    liblcms2-dev \
    libgnutls28-dev \
    libdbus-1-dev \
    libsystemd-dev \
    libxml2-dev \
    libsqlite3-dev \
    libtree-sitter-dev \
    libasound2-dev \
    libgmp-dev \
    libacl1-dev \
    libseccomp-dev \
    libselinux1-dev \
    libgpm-dev \
    libpoppler-glib-dev

  sudo apt autoremove --purge
  step_done "Build packages removed."
}

step_post_install_checks() {
  divider "Step 10 — Post-install checks"

  local started_temp_daemon=0
  if ! emacsclient --eval 't' &>/dev/null; then
    emacs --daemon
    started_temp_daemon=1
  fi

  local backend native_comp fd_binary path_ok=0 cargo_ok=0

  native_comp=$(emacsclient --eval '(if (and (fboundp (quote native-comp-available-p)) (native-comp-available-p)) "t" "nil")' 2>/dev/null | tr -d '"')
  [[ "$native_comp" == "t" ]] \
    && step_done "Native compilation" \
    || step_fail "Native compilation unavailable"


  [[ "$started_temp_daemon" -eq 1 ]] && emacsclient --eval '(kill-emacs)' &>/dev/null || true
}

# ── summary ───────────────────────────────────────────────────────────────────
print_summary() {
  divider "Setup complete"
  log "vterm, ghostel, whisper.cpp installed in Step 8; checked in Step 10."
}

# ── main ──────────────────────────────────────────────────────────────────────
[[ "$PLATFORM" == "windows" ]] && die "Interactive mode is Linux-only. Run with --windows for Windows deps."

divider "Doom Emacs — Interactive Setup"
log "Platform : $PLATFORM"
log "User     : $USER"
log "Home     : $HOME"
echo ""
log "Each step will ask for confirmation before running."
log "You can safely skip steps you have already completed."
echo ""

step_apt_deps
step_languagetool
step_rust
step_npm
step_build_emacs
step_doom
step_systemd
step_first_run_setup
step_cleanup
step_post_install_checks
print_summary
