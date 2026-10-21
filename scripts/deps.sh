#!/usr/bin/env bash
# deps.sh — Interactive install of the external tools the Doom config uses
# (language servers, search tools, LaTeX, mail, grammar checker, ...).
#
# Setup order on a fresh machine: deps.sh → build.sh → install.sh
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"
# shellcheck source=lib/common.sh
source "${SCRIPT_DIR}/lib/common.sh"

# ── step definitions ──────────────────────────────────────────────────────────

step_apt_deps() {
    divider "Step 1 — System packages (apt)"
    log "This installs the runtime tools used by the Doom modules."
    log "Requires: sudo, apt"
    echo ""

    ask "Install system packages?" || {
        step_skip "apt packages"
        return 0
    }

    sudo apt update
    # build-essential, cmake, libtool-bin and the -dev packages here are for
    # the native modules Doom packages compile themselves (vterm, jinx,
    # pdf-tools, whisper.cpp), not for building Emacs — see build.sh.
    sudo apt install -y \
        build-essential \
        clangd \
        cmake \
        curl \
        default-jre \
        fd-find \
        ffmpeg \
        ffmpegthumbnailer \
        gdb \
        git \
        graphviz \
        imagemagick \
        isync \
        libenchant-2-dev \
        libpoppler-glib-dev \
        libtool-bin \
        libvterm-dev \
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
        texlive-fonts-extra \
        texlive-fonts-recommended \
        texlive-latex-base \
        texlive-latex-extra \
        unzip \
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
        ask "Reinstall?" "n" || {
            step_skip "LanguageTool"
            return 0
        }
        rm -rf "${LT_DIR:?}/LanguageTool-${LT_VERSION}"
    else
        ask "Install LanguageTool ${LT_VERSION}?" || {
            step_skip "LanguageTool"
            return 0
        }
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
        ask "Update and reinstall components anyway?" "n" || {
            step_skip "Rust"
            return 0
        }
    else
        ask "Install Rust toolchain?" || {
            step_skip "Rust"
            return 0
        }
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

    ask "Install npm packages?" || {
        step_skip "npm packages"
        return 0
    }

    NPM_PREFIX="${HOME}/.local"
    npm config set prefix "${NPM_PREFIX}"
    export PATH="${NPM_PREFIX}/bin:${PATH}"

    npm install -g pyright

    step_done "npm packages installed."
    log "Make sure ~/.local/bin is in your PATH:"
    log "  export PATH=\"\$HOME/.local/bin:\$PATH\""
}

# ── main ──────────────────────────────────────────────────────────────────────
# When sourced (e.g. by the tests), only define the steps above.
[[ "${BASH_SOURCE[0]}" == "$0" ]] || return 0

require_linux
setup_banner "Doom Emacs — External Dependencies"

step_apt_deps
step_languagetool
step_rust
step_npm

divider "Dependencies done"
log "Next: scripts/build.sh to build Emacs, or scripts/install.sh if it is already installed."
