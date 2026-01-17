#!/bin/bash
# Dotfiles installation script for bare repo method
# Usage: curl -Lks https://raw.githubusercontent.com/alecf/shellconfig/main/install.sh | bash

set -e

echo "Installing dotfiles..."
echo ""

# Check bash version
BASH_MAJOR_VERSION=${BASH_VERSION%%.*}
if [ "$BASH_MAJOR_VERSION" -lt 4 ]; then
    echo "⚠️  Warning: Your bash version is $BASH_VERSION"
    echo "   These dotfiles work best with bash 4+."
    if [ "$(uname)" = "Darwin" ]; then
        echo "   Install newer bash with: brew install bash"
    fi
    echo ""
fi

# Check if git is installed
if ! command -v git &> /dev/null; then
    echo "❌ Error: git is not installed"
    echo "   Install git and try again"
    exit 1
fi

# Clone the bare repo
if [ ! -d "$HOME/.cfg" ]; then
    echo "📦 Cloning dotfiles repository..."
    git clone --bare https://github.com/alecf/shellconfig.git "$HOME/.cfg"
else
    echo "📦 Dotfiles repository already exists at ~/.cfg"
fi

# Define the config alias temporarily for this script
function config {
    /usr/bin/git --git-dir="$HOME/.cfg/" --work-tree="$HOME" "$@"
}

# Checkout the actual files
echo "📝 Checking out dotfiles..."
if ! config checkout 2>/dev/null; then
    echo "⚠️  Backing up pre-existing dotfiles..."
    mkdir -p "$HOME/.dotfiles-backup"
    config checkout 2>&1 | grep -E "\s+\." | awk '{print $1}' | xargs -I{} mv {} "$HOME/.dotfiles-backup/{}" 2>/dev/null || true
    echo "   Backed up conflicting files to ~/.dotfiles-backup/"
    config checkout
fi

# Configure git to not show untracked files
config config --local status.showUntrackedFiles no

echo ""
echo "✅ Dotfiles installed successfully!"
echo ""

# Check for Starship and offer to install
if ! command -v starship &> /dev/null; then
    echo "🚀 Starship prompt not found"
    echo ""

    # Detect OS and package manager
    if [ "$(uname)" = "Darwin" ]; then
        if command -v brew &> /dev/null; then
            read -p "   Install Starship via Homebrew? [y/N] " -n 1 -r </dev/tty
            echo
            if [[ $REPLY =~ ^[Yy]$ ]]; then
                brew install starship
                echo "✅ Starship installed!"
            else
                echo "   You can install it later with: brew install starship"
            fi
        else
            echo "   Install Homebrew first: https://brew.sh"
            echo "   Then run: brew install starship"
        fi
    elif [ "$(uname)" = "Linux" ]; then
        read -p "   Install Starship? [y/N] " -n 1 -r </dev/tty
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            curl -sS https://starship.rs/install.sh | sh
            echo "✅ Starship installed!"
        else
            echo "   You can install it later: curl -sS https://starship.rs/install.sh | sh"
        fi
    fi
    echo ""
else
    echo "✅ Starship is already installed"
    echo ""
fi

echo "The 'config' command is available after restarting your shell."
echo ""
echo "Usage:"
echo "  config status          # Show status of tracked dotfiles"
echo "  config add <file>      # Track a new dotfile"
echo "  config commit -m 'msg' # Commit changes"
echo "  config push            # Push to GitHub"
echo ""
echo "🎉 Setup complete! Restart your shell or run: source ~/.bashrc"
