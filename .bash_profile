# Set PATH, MANPATH, etc., for Homebrew (must be before .bashrc)
if [ -f /opt/homebrew/bin/brew ]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi

. ~/.bashrc
