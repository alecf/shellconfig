export BASH_SILENCE_DEPRECATION_WARNING=1

if [ -d "$HOME/.linuxbrew" ]; then
    eval $("$HOME/.linuxbrew/bin/brew" shellenv)
fi

if [ -x "$(command -v brew)" ]; then
    BREW_PREFIX=$(brew --prefix)
else
    BREW_PREFIX=
fi

if [ -d "$BREW_PREFIX/opt/postgresql@15" ]; then
    export PATH="$BREW_PREFIX/opt/postgresql@15/bin:$PATH"
fi

# Guarded initialization: avoid running init twice in the same shell
if [ -z "$PYENV_INITIALIZED" ] && command -v pyenv >/dev/null 2>&1; then
    export PYENV_INITIALIZED=1
    export PYENV_ROOT="$HOME/.pyenv"
    export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init -)"
    if command -v virtualenv-init &>/dev/null; then
        eval "$(pyenv virtualenv-init -)"
    fi
    # Adds the shims/ directory to the path
    eval "$(pyenv init --path)"
fi

# from http://superuser.com/questions/39751/add-directory-to-path-if-its-not-already-there
add_to_path() {
    if [[ "$PATH" =~ (^|:)"${1}"(:|$) ]]; then
        return 0
    fi
    export PATH=${1}:$PATH
}

# If not running interactively, don't do anything
case $- in
*i*) ;;
*) return ;;
esac

# Check for starship once, reuse throughout
if command -v starship &> /dev/null; then
    HAS_STARSHIP=1
else
    HAS_STARSHIP=
fi

# Terminal title (only when not using starship, which handles titles itself)
if [ -z "$HAS_STARSHIP" ]; then
    PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}:${PWD}\007"'
    if [ -n "$(type -t update_terminal_cwd)" ]; then
        PROMPT_COMMAND="$PROMPT_COMMAND;update_terminal_cwd"
    fi
fi

# add_to_path /usr/local/opt/gettext/bin
# add_to_path /usr/local/opt/coreutils/libexec/gnubin
add_to_path ~/bin
add_to_path ~/.local/bin

# Prompt configuration: use Starship if available, otherwise a simple fallback
if [ -n "$HAS_STARSHIP" ]; then
    eval "$(starship init bash)"
else
    # Simple fallback prompt: user@host:dir $
    PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\W\[\033[00m\]\$ '
fi

if [[ "$TERM_PROGRAM" != "vscode" && "$TERM_PROGRAM" != "cursor" ]]; then
    export PAGER="less -r"
fi
export LESS="-R -S -X"

export GPG_TTY=$(tty)
export AWS_VAULT_BACKEND=pass

# Autocomplete
if [ -f $BREW_PREFIX/etc/bash_completion ]; then
    . $BREW_PREFIX/etc/bash_completion
fi

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
if command -v lesspipe &> /dev/null; then
    eval "$(SHELL=/bin/sh lesspipe)"
elif command -v lesspipe.sh &> /dev/null; then
    eval "$(SHELL=/bin/sh lesspipe.sh)"
fi

# If this is an xterm set the title to user@host:dir (only when not using starship)
if [ -z "$HAS_STARSHIP" ]; then
    case "$TERM" in
    xterm* | rxvt*)
        PS1="\[\e]0;\u@\h: \w\a\]$PS1"
        ;;
    *) ;;
    esac
fi

# Cross-platform colored ls and grep
if [ "$(uname)" = "Darwin" ]; then
    alias ls='ls -G'
else
    # Linux: set up dircolors if available
    if command -v dircolors &> /dev/null; then
        test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    fi
    alias ls='ls --color=auto'
fi
alias grep='grep --color=auto'

# ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        . /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    fi
fi

alias meld=/Applications/Meld.app/Contents/MacOS/meld

if [ -x "$(command -v rbenv)" ]; then
    eval "$(rbenv init -)"
fi

if [ -x $HOME/bin/src-hilite-lesspipe.sh ]; then
    export LESSOPEN="|$HOME/bin/src-hilite-lesspipe.sh %s"
fi

## from http://stackoverflow.com/questions/3231804/in-bash-how-to-add-are-you-sure-y-n-to-any-command-or-alias
confirm() {
    # call with a prompt string or use a default
    read -r -p "${1:-Are you sure? [y/N]} " response
    case $response in
    [yY][eE][sS] | [yY])
        true
        ;;
    *)
        false
        ;;
    esac
}

alias noorig="files=\$(find . -name '*.orig') ; echo Remove \$files? ; confirm && rm -f \$files"
#>>agr "search" "replace"
agr() {
    ag "$1" --nogroup | awk '{print substr($1,1,index($1,":")-1);}' | xargs -I {} sed -i '' -e "s/$1/$2/g" {}
}

_eslintd() {
    git diff --name-only --relative $* | grep -E '\.[jt]sx?$' | xargs -t ./node_modules/.bin/eslint --fix
}
_tslintd() {
    git diff --name-only --relative $* | grep -E '\.tsx?$' | xargs -t ./node_modules/.bin/tslint --fix -p .
}
_precommitd() {
    git diff --name-only --relative $* | xargs -t pre-commit run --files
}
alias gbage='for k in `git branch --format "%(refname:short)"`; do echo -e "`git log -1 --color --pretty=format:"%Cgreen%ci %Cblue%<(13,trunc)%cr %C(yellow bold)%<(30,trunc)%S %Creset%<(40,trunc)%D" $k --`"; done | sort -r'

alias eslintd=_eslintd
alias tslintd=_tslintd
alias precommitd=_precommitd
alias dc=docker-compose
alias dclf='COMPOSE_HTTP_TIMEOUT=10000 docker-compose logs --tail=20 -f'
alias dc-lint-watch='docker-compose exec frontend ./node_modules/.bin/watch "time ./node_modules/.bin/eslint --color --cache uf" uf'
alias dc-lint='docker-compose exec frontend ./node_modules/.bin/eslint --color --cache uf'
alias pcommit='git diff --name-only --relative master... | xargs -t pre-commit run --files'
alias noansi="sed 's/\x1b\[[0-9;]*[a-zA-Z]//g'"
alias ave="aws-vault exec"
alias aved="ave dev --"

if [ "$(uname)" == "Darwin" ]; then
    ulimit -n 65536 65536
fi

if [ "$(uname)" != "Darwin" ]; then
    export AWS_VAULT_BACKEND=pass
fi

# NVM, so we don't break the system copy, such as homebrew
if [ -f "$HOME/.nvm/nvm.sh" ]; then
    export NVM_DIR="$HOME/.nvm"
    . "$NVM_DIR/nvm.sh"
fi

aws_mfa_iam="arn:aws:iam::043731723972:mfa/alecf"
alias awslogin="read -p 'Enter MFA Code:' mfa && echo \$(aws-auth --serial-number $aws_mfa_iam --token-code \$mfa)"

# pbpaste | findfailures
alias findfailures="jq -r '.suites[].cases[] | select(.status==\"FAILED\") | .className' | sort -u"

alias dockertty="screen ~/Library/Containers//com.docker.docker/Data/vms/0/tty"
gitcommitlast() {
    git diff --name-only --relative $* | xargs -I== git log -1 --format=format:"git commit == --no-verify -m 'squash with %h (%s)'%n" == | cat
}
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion" # This loads nvm bash_completion
export VOLTA_HOME="$HOME/.volta"
export PATH="$VOLTA_HOME/bin:$PATH"

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH=$BUN_INSTALL/bin:$PATH

# pnpm
export PNPM_HOME="/Users/alecf/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end

# Dotfiles management with bare git repo
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
