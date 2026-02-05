#!/usr/bin/env sh
set -euo pipefail

ROOT="$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)"
SRC="$ROOT/AGENTS.md"
DST="$ROOT/CLAUDE.md"

if [ ! -f "$SRC" ]; then
  echo "Missing AGENTS.md at $SRC" >&2
  exit 1
fi

header="$(sed -n '1p' "$SRC")"
intro="$(sed -n '3p' "$SRC")"

if [ "$header" != "# AGENTS.md" ]; then
  echo "Unexpected AGENTS.md header: $header" >&2
  exit 1
fi

if [ -z "$intro" ]; then
  echo "AGENTS.md intro line is missing." >&2
  exit 1
fi

generate() {
  cat <<'EOF'
# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

EOF
  sed -n '5,$p' "$SRC"
}

if [ "${1:-}" = "--check" ]; then
  tmp="$(mktemp "${TMPDIR:-/tmp}/harp-claude.XXXXXX")"
  generate > "$tmp"
  if ! cmp -s "$tmp" "$DST"; then
    echo "CLAUDE.md is out of sync with AGENTS.md." >&2
    diff -u "$DST" "$tmp" >&2 || true
    rm -f "$tmp"
    exit 1
  fi
  rm -f "$tmp"
  exit 0
fi

generate > "$DST"
