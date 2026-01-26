#!/bin/bash
# Session start hook for setting up Haskell development environment
# This script installs GHCup and Stack. GHC is managed by Stack based on resolver.

set -e

GHCUP_DIR="$HOME/.ghcup"
GHCUP_BIN="$GHCUP_DIR/bin"

# Function to add PATH to CLAUDE_ENV_FILE for persistence
setup_path() {
    if [ -n "$CLAUDE_ENV_FILE" ]; then
        echo "export PATH=\"$GHCUP_BIN:\$PATH\"" >> "$CLAUDE_ENV_FILE"
    fi
    export PATH="$GHCUP_BIN:$PATH"
}

# Check if GHCup is already installed
if [ -x "$GHCUP_BIN/ghcup" ]; then
    echo "GHCup is already installed"
    setup_path

    # Verify stack is available
    if command -v stack &> /dev/null; then
        echo "Stack is available: $(stack --version)"
    else
        echo "Installing Stack via GHCup..."
        ghcup install stack --set
    fi

    exit 0
fi

echo "Installing GHCup (Haskell toolchain installer)..."

# Install GHCup non-interactively (Stack only, GHC managed by Stack)
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_INSTALL_STACK=1 \
    BOOTSTRAP_HASKELL_INSTALL_GHC=0 \
    BOOTSTRAP_HASKELL_INSTALL_HLS=0 \
    BOOTSTRAP_HASKELL_ADJUST_BASHRC=0 \
    sh

# Set up PATH
setup_path

echo "Haskell environment setup complete!"
echo "Stack: $(stack --version)"
echo "Note: GHC will be installed by Stack on first build based on resolver"

exit 0
