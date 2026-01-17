# Dotfiles

My personal dotfiles using the bare git repository method.

## Features

- **Starship prompt** with git status indicators
- **Cross-platform** bash configuration (macOS + Linux)
- **Clean separation** - only explicitly tracked files are managed
- **No symlinks** - files live in their natural locations
- **Simple management** - use `config` command instead of `git`

## Quick Install

```bash
curl -Lks https://raw.githubusercontent.com/alecf/shellconfig/main/install.sh | bash
```

Then restart your shell or run `source ~/.bashrc`.

## Manual Install

```bash
# Clone the bare repository
git clone --bare https://github.com/alecf/shellconfig.git $HOME/.cfg

# Define the config alias
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

# Checkout dotfiles
config checkout

# Hide untracked files
config config --local status.showUntrackedFiles no

# Reload shell
source ~/.bashrc
```

## Usage

Use the `config` command exactly like `git`, but for dotfiles:

```bash
# Check status
config status

# Add a new dotfile
config add .vimrc

# Commit changes
config commit -m "Update vimrc"

# Push to GitHub
config push

# Pull latest changes
config pull
```

## What's Included

- `.bashrc` - Main bash configuration
- `.bash_profile` - macOS login shell setup
- `.profile` - Linux login shell setup
- `.gitconfig` - Git configuration with aliases
- `.config/starship.toml` - Starship prompt configuration
- `bin/` - Personal scripts

## Key Bash Features

- **Starship prompt** with simple fallback
- **Cross-platform ls/grep** aliases (works on macOS and Linux)
- **Version managers**: pyenv, rbenv, nvm, volta
- **Tool support**: docker-compose, aws-vault, pre-commit
- **Git aliases**: gbage (branch age), cleanup-main, cleanup-merged

## Requirements

- Git
- Bash 4+ (macOS ships with 3.2, use Homebrew to upgrade)
- [Starship](https://starship.rs/) for the fancy prompt (optional)

## Adding New Files

To track a new dotfile:

```bash
config add -f ~/.newfile
config commit -m "Add newfile"
config push
```

The `-f` flag is needed because `.gitignore` ignores everything by default.

## Platform-Specific Config

The `.bashrc` detects the OS and adjusts automatically:

- **macOS**: Uses `ls -G` for colors, loads Homebrew
- **Linux**: Uses `ls --color=auto`, checks for linuxbrew

## Secrets Management

**Never commit secrets!** For machine-specific credentials:

1. Create `~/.bash_local` for environment variables
2. Source it from `.bashrc` (add `[ -f ~/.bash_local ] && . ~/.bash_local`)
3. Add `!.bash_local` to `.gitignore` (already done)

## Troubleshooting

**Q: `config` command not found**
A: Reload your shell with `source ~/.bashrc` or start a new shell.

**Q: Files won't checkout due to conflicts**
A: The install script backs up existing files to `~/.dotfiles-backup/`.

**Q: How do I see all tracked files?**
A: Run `config ls-files`

**Q: Starship not showing git status**
A: Make sure Starship is installed: `brew install starship` (macOS) or check [starship.rs](https://starship.rs/)

## Why Bare Repo?

The bare repository method is the modern standard for dotfiles because:

1. **No symlinks** - files stay where they belong
2. **Safe by default** - won't accidentally commit everything in `~/`
3. **Works everywhere** - just git, no special tools needed
4. **Clean `git status`** in your home directory still works for other repos

## License

Do whatever you want with this.
