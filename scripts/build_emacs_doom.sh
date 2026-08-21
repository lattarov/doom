#!/usr/bin/env bash
# install-doom.sh — Interactive Doom Emacs setup
set -euo pipefail

# ── helpers ───────────────────────────────────────────────────────────────────
log()     { echo "[INFO]  $*"; }
warn()    { echo "[WARN]  $*" >&2; }
die()     { echo "[ERROR] $*" >&2; exit 1; }
divider() { echo ""; echo "════════════════════════════════════════════════════════"; echo "  $*"; echo "════════════════════════════════════════════════════════"; echo ""; }

ask() {
  # ask "Question?" => returns 0 for yes, 1 for no
  local prompt="$1"
  local default="${2:-y}"
  local yn
  if [[ "$default" == "y" ]]; then
    read -r -p "  $prompt [Y/n] " yn
    yn="${yn:-y}"
  else
    read -r -p "  $prompt [y/N] " yn
    yn="${yn:-n}"
  fi
  [[ "$yn" =~ ^[Yy]$ ]]
}

step_done()   { echo "  ✔  $*"; }
step_skip()   { echo "  ─  $* (skipped)"; }
step_fail()   { echo "  ✘  $*" >&2; }

# ── platform detection ────────────────────────────────────────────────────────
if [[ "${OS:-}" == "Windows_NT" ]]; then
  PLATFORM="windows"
elif [[ "$(uname)" == "Linux" ]]; then
  PLATFORM="linux"
else
  die "Unsupported platform."
fi

# ── corporate network state ───────────────────────────────────────────────────
# Pre-seed from the environment so a non-interactive run (CORP_PROXY=...
# CORP_CA_BUNDLE=... ./build_emacs_doom.sh) skips the prompts entirely.
CORP_PROXY="${CORP_PROXY:-${https_proxy:-${HTTPS_PROXY:-}}}"
CORP_CA_BUNDLE="${CORP_CA_BUNDLE:-}"
ENV_D_DIR="${HOME}/.config/environment.d"
ENV_D_FILE="${ENV_D_DIR}/10-corporate-proxy.conf"

# ── step definitions ──────────────────────────────────────────────────────────

step_corporate_network() {
  divider "Step 0 — Corporate proxy / CA trust"
  log "If you're behind a corporate HTTP(S) proxy or a TLS-inspecting"
  log "firewall, configure it here once. Everything downstream — apt, git"
  log "(and therefore straight.el's package clones), cargo, npm, and the"
  log "Emacs daemon — inherits it from this step."
  echo ""

  if [[ -z "$CORP_PROXY" ]]; then
    if ask "Are you behind a corporate HTTP(S) proxy?" "n"; then
      read -r -p "  Proxy URL (e.g. http://proxy.corp.example:8080): " CORP_PROXY
    fi
  else
    log "Using proxy from environment: ${CORP_PROXY}"
  fi

  if [[ -n "$CORP_PROXY" ]]; then
    local no_proxy_val="${no_proxy:-${NO_PROXY:-localhost,127.0.0.1}}"
    export http_proxy="$CORP_PROXY" https_proxy="$CORP_PROXY"
    export HTTP_PROXY="$CORP_PROXY" HTTPS_PROXY="$CORP_PROXY"
    export no_proxy="$no_proxy_val" NO_PROXY="$no_proxy_val"

    log "git: setting http.proxy/https.proxy globally. Emacs always reads"
    log "~/.gitconfig when it shells out to git, regardless of the"
    log "daemon's own environment — this is what makes straight.el's"
    log "package clones work behind the proxy."
    git config --global http.proxy "$CORP_PROXY"
    git config --global https.proxy "$CORP_PROXY"

    log "apt: writing a proxy conf (sudo drops env vars by default, so"
    log "apt needs its own copy)."
    printf 'Acquire::http::Proxy "%s";\nAcquire::https::Proxy "%s";\n' \
      "$CORP_PROXY" "$CORP_PROXY" \
      | sudo tee /etc/apt/apt.conf.d/95corporate-proxy >/dev/null

    log "cargo: writing ~/.cargo/config.toml [http] proxy."
    mkdir -p "${HOME}/.cargo"

    log "systemd: persisting proxy vars for your user session, so"
    log "services started later (emacs.service) inherit them too, and"
    log "applying it to the current session immediately."
    mkdir -p "$ENV_D_DIR"
    {
      echo "http_proxy=${CORP_PROXY}"
      echo "https_proxy=${CORP_PROXY}"
      echo "HTTP_PROXY=${CORP_PROXY}"
      echo "HTTPS_PROXY=${CORP_PROXY}"
      echo "no_proxy=${no_proxy_val}"
      echo "NO_PROXY=${no_proxy_val}"
    } > "$ENV_D_FILE"
    systemctl --user set-environment \
      http_proxy="$CORP_PROXY" https_proxy="$CORP_PROXY" \
      HTTP_PROXY="$CORP_PROXY" HTTPS_PROXY="$CORP_PROXY" \
      no_proxy="$no_proxy_val" NO_PROXY="$no_proxy_val" 2>/dev/null || true

    step_done "Proxy configured: ${CORP_PROXY}"
  else
    step_skip "proxy configuration"
  fi

  if [[ -z "$CORP_CA_BUNDLE" ]]; then
    if ask "Does your network use a TLS-inspecting proxy with a custom root CA?" "n"; then
      read -r -p "  Path to the CA certificate (PEM): " CORP_CA_BUNDLE
    fi
  else
    log "Using CA bundle from environment: ${CORP_CA_BUNDLE}"
  fi

  if [[ -n "$CORP_CA_BUNDLE" ]]; then
    [[ -f "$CORP_CA_BUNDLE" ]] || die "CA bundle not found at ${CORP_CA_BUNDLE}"

    log "Installing into the system trust store — this alone covers git,"
    log "curl, wget, and Emacs's own GnuTLS-based TLS stack (url.el,"
    log "gnutls-cli), since they all consult the OS trust store."
    sudo cp "$CORP_CA_BUNDLE" /usr/local/share/ca-certificates/corporate-root-ca.crt
    sudo update-ca-certificates

    log "Also setting explicit CA vars for tools that don't trust the"
    log "system store by default (node/npm, cargo, pip/requests)."
    export NODE_EXTRA_CA_CERTS="$CORP_CA_BUNDLE"
    export SSL_CERT_FILE="$CORP_CA_BUNDLE"
    export REQUESTS_CA_BUNDLE="$CORP_CA_BUNDLE"
    export CARGO_HTTP_CAINFO="$CORP_CA_BUNDLE"

    {
      echo "NODE_EXTRA_CA_CERTS=${CORP_CA_BUNDLE}"
      echo "SSL_CERT_FILE=${CORP_CA_BUNDLE}"
      echo "REQUESTS_CA_BUNDLE=${CORP_CA_BUNDLE}"
      echo "CARGO_HTTP_CAINFO=${CORP_CA_BUNDLE}"
    } >> "$ENV_D_FILE"
    systemctl --user set-environment \
      NODE_EXTRA_CA_CERTS="$CORP_CA_BUNDLE" SSL_CERT_FILE="$CORP_CA_BUNDLE" \
      REQUESTS_CA_BUNDLE="$CORP_CA_BUNDLE" CARGO_HTTP_CAINFO="$CORP_CA_BUNDLE" 2>/dev/null || true

    step_done "Corporate CA trusted (system store + tool-specific env vars)."
  else
    step_skip "custom CA trust"
  fi

  # Single write pass for cargo's [http] table — avoids duplicate-header
  # TOML errors if both proxy and CA bundle were configured above.
  if [[ -n "$CORP_PROXY" || -n "$CORP_CA_BUNDLE" ]] \
     && ! grep -q '^\[http\]' "${HOME}/.cargo/config.toml" 2>/dev/null; then
    mkdir -p "${HOME}/.cargo"
    {
      echo "[http]"
      [[ -n "$CORP_PROXY" ]] && echo "proxy = \"${CORP_PROXY}\""
      [[ -n "$CORP_CA_BUNDLE" ]] && echo "cainfo = \"${CORP_CA_BUNDLE}\""
    } >> "${HOME}/.cargo/config.toml"
  fi
}

step_apt_deps() {
  divider "Step 1 — System packages (apt)"
  log "This installs all build and runtime dependencies."
  log "Requires: sudo, apt, gcc"
  echo ""

  ask "Install system packages?" || { step_skip "apt packages"; return 0; }

  GCC_VER=$(gcc -dumpversion | cut -d. -f1)
  sudo apt update
  sudo apt install -y \
    fd-find \
    git \
    ripgrep \
    build-essential \
    autoconf \
    texinfo \
    libgccjit0 \
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
    sqlite3 \
    libtree-sitter-dev \
    libasound2-dev \
    libgmp-dev \
    libacl1-dev \
    libseccomp-dev \
    libselinux1-dev \
    libgpm-dev \
    isync \
    maildir-utils \
    graphviz \
    pandoc \
    maim \
    wl-clipboard \
    texlive-latex-base \
    texlive-fonts-recommended \
    texlive-fonts-extra \
    texlive-latex-extra \
    cmake \
    libvterm-dev \
    libtool-bin \
    clangd \
    npm \
    ffmpegthumbnailer \
    mediainfo \
    imagemagick \
    poppler-utils \
    plantuml \
    default-jre \
    ffmpeg \
    python3-pip \
    libpoppler-glib-dev \
    gdb

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

  if [[ -n "$CORP_PROXY" ]]; then
    npm config set proxy "$CORP_PROXY"
    npm config set https-proxy "$CORP_PROXY"
  fi
  if [[ -n "$CORP_CA_BUNDLE" ]]; then
    npm config set cafile "$CORP_CA_BUNDLE"
  fi

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
    | grep -v '\.' \
    | sort -V \
    | tail -1 \
    || echo "emacs-30.2")  # fallback if network unavailable

  echo "       → ${LATEST_STABLE} (detected)"
  echo ""
  echo "    2) Master branch (development — currently Emacs 31.x)"
  echo "       → unstable, latest features, may have bugs"
  echo ""
  read -r -p "  Choice [1/2, default 1]: " version_choice
  version_choice="${version_choice:-1}"

  case "$version_choice" in
    2)
      GIT_REF="master"
      GIT_BRANCH_FLAG="--depth=1"
      log "Selected: master (development)"
      ;;
    *)
      GIT_REF="${LATEST_STABLE}"
      GIT_BRANCH_FLAG="--depth=1 --branch ${LATEST_STABLE}"
      log "Selected: ${LATEST_STABLE} (stable)"
      ;;
  esac

  log ""
  log "  Source dir : ${EMACS_SRC:-$HOME/dev/emacs}"
  log "  Git ref    : ${GIT_REF}"
  log "  Build jobs : $(nproc --ignore=1)"
  log "  Install to : /usr/local"
  log ""
  warn "This will take several minutes."

  ask "Build and install Emacs?" || { step_skip "Emacs build"; return 0; }

  EMACS_SRC="${EMACS_SRC:-$HOME/dev/emacs}"
  JOBS=$(nproc --ignore=1)

  if [[ -d "$EMACS_SRC" ]]; then
    log "Source directory exists at ${EMACS_SRC}."
    CURRENT_REF=$(git -C "$EMACS_SRC" describe --tags 2>/dev/null \
                  || git -C "$EMACS_SRC" rev-parse --abbrev-ref HEAD)
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

  cd "$EMACS_SRC"

  # stable tags need autogen; master ships a pre-generated configure
  log "Running autogen..."
  ./autogen.sh

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

  DOOM_DIR="${XDG_CONFIG_HOME:-$HOME/.config}/emacs"

  if [[ -d "$DOOM_DIR" ]]; then
    log "Doom already present at ${DOOM_DIR}."
    ask "Run doom sync?" || { step_skip "doom sync"; return 0; }
    "${DOOM_DIR}/bin/doom" sync
    step_done "doom sync complete."
  else
    ask "Install Doom Emacs?" || { step_skip "Doom install"; return 0; }
    git clone --depth=1 https://github.com/doomemacs/doomemacs "$DOOM_DIR"
    "${DOOM_DIR}/bin/doom" install
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

# ── summary ───────────────────────────────────────────────────────────────────
print_summary() {
  divider "Setup complete"
  log "vterm, ghostel, whisper.cpp were installed in Step 8."
  log ""
  log "Verify Wayland rendering:"
  log "  M-: (pgtk-backend-display-class)"
  log "  expected: GdkWaylandDisplay"
  log ""
  log "Verify native compilation:"
  log "  M-: (native-comp-available-p)"
  log "  expected: t"
  log ""
  log "fdfind note — add to config.el:"
  log "  (setq doom-projectile-fd-binary (executable-find \"fdfind\"))"
  log ""
  log "PATH note — add to ~/.bashrc or ~/.zshrc if not present:"
  log "  export PATH=\"\$HOME/.local/bin:\$PATH\""
  log "  source \$HOME/.cargo/env"
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
print_summary
