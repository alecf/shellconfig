#!/bin/bash
# Dotfiles installation script for bare repo method
# Usage: curl -Lks https://raw.githubusercontent.com/alecf/shellconfig/main/install.sh | bash

set -e

echo "Installing dotfiles..."

# Clone the bare repo
if [ ! -d "$HOME/.cfg" ]; then
    echo "Cloning dotfiles repository..."
    git clone --bare https://github.com/alecf/shellconfig.git "$HOME/.cfg"
else
    echo "Dotfiles repository already exists at ~/.cfg"
fi

# Define the config alias temporarily for this script
function config {
    /usr/bin/git --git-dir="$HOME/.cfg/" --work-tree="$HOME" "$@"
}

# Checkout the actual files
echo "Checking out dotfiles..."
if ! config checkout; then
    echo "Backing up pre-existing dotfiles..."
    mkdir -p "$HOME/.dotfiles-backup"
    config checkout 2>&1 | grep -E "\s+\." | awk '{print $1}' | xargs -I{} mv {} "$HOME/.dotfiles-backup/{}"
    echo "Backed up conflicting files to ~/.dotfiles-backup/"
    config checkout
fi

# Configure git to not show untracked files
config config --local status.showUntrackedFiles no

echo ""
echo "✓ Dotfiles installed successfully!"
echo ""
echo "The 'config' command is available after restarting your shell."
echo "Usage:"
echo "  config status          # Show status of tracked dotfiles"
echo "  config add <file>      # Track a new dotfile"
echo "  config commit -m 'msg' # Commit changes"
echo "  config push            # Push to GitHub"
echo ""
echo "Restart your shell or run: source ~/.bashrc"
