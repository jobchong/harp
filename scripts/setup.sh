#!/usr/bin/env sh
set -euo pipefail

ROOT="$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)"
cd "$ROOT"

usage() {
  cat <<'EOF'
Usage: scripts/setup.sh [--install-emacs] [--byte-compile]

--install-emacs   Attempt to install Emacs with the detected package manager.
--byte-compile    Run byte-compilation after setup.
EOF
}

install_emacs=false
byte_compile=false

for arg in "$@"; do
  case "$arg" in
    --install-emacs) install_emacs=true ;;
    --byte-compile) byte_compile=true ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $arg" >&2
      usage
      exit 1
      ;;
  esac
done

ensure_emacs() {
  if command -v emacs >/dev/null 2>&1; then
    return 0
  fi

  if [ "$install_emacs" != "true" ]; then
    echo "Emacs is not installed. Install it or rerun with --install-emacs." >&2
    exit 1
  fi

  case "$(uname -s)" in
    Darwin)
      if command -v brew >/dev/null 2>&1; then
        brew install emacs
      else
        echo "Homebrew not found. Install Emacs manually or install Homebrew first." >&2
        exit 1
      fi
      ;;
    Linux)
      if command -v apt-get >/dev/null 2>&1; then
        sudo apt-get update
        sudo apt-get install -y emacs
      elif command -v dnf >/dev/null 2>&1; then
        sudo dnf install -y emacs
      elif command -v pacman >/dev/null 2>&1; then
        sudo pacman -S --noconfirm emacs
      else
        echo "Unsupported package manager. Install Emacs manually." >&2
        exit 1
      fi
      ;;
    *)
      echo "Unsupported OS. Install Emacs manually." >&2
      exit 1
      ;;
  esac
}

ensure_emacs

git config core.hooksPath scripts/githooks

./scripts/sync-agent-docs.sh

if [ "$byte_compile" = "true" ]; then
  emacs -Q -batch -L . -f batch-byte-compile *.el
  rm -f *.elc
fi

echo "Setup complete."
