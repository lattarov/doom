#!/usr/bin/env bash
# colors.sh — Color scheme manager for scripts/*.sh output.
#
# Honors the NO_COLOR convention (https://no-color.org) and disables color
# codes automatically when stdout isn't a terminal (e.g. piped to a log).

if [[ -n "${NO_COLOR:-}" ]] || [[ ! -t 1 ]]; then
  GREEN=""
  RED=""
  YELLOW=""
  RESET=""
else
  GREEN='\033[0;32m'
  RED='\033[0;31m'
  YELLOW='\033[1;33m'
  RESET='\033[0m'
fi
