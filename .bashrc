if [ -x "$(command -v brew)" ]; then
    BREW_PREFIX=`brew --prefix`
else
    BREW_PREFIX=
fi

if [ -d "$HOME/.pyenv" ]; then
  export PYENV_ROOT="$HOME/.pyenv"
  export PATH="$PYENV_ROOT/bin:$PATH"
  eval "$(pyenv init -)"
  eval "$(pyenv virtualenv-init -)"
fi



# from http://superuser.com/questions/39751/add-directory-to-path-if-its-not-already-there
add_to_path ()
{
    if [[ "$PATH" =~ (^|:)"${1}"(:|$) ]]
    then
        return 0
    fi
    export PATH=${1}:$PATH
}

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

xtitle ()
{
    echo -ne "\033];$*\007"
}

path_tail ()
{
    shortpath=$(pwd|awk -F/ '{print $(NF-1) "/" $NF}')
    echo -n $shortpath
#    xtitle $shortpath
}

# Need to just incorporate this into PS1
PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}:${PWD}\007"'

if [ -n "$(type -t update_terminal_cwd)" ]; then
    PROMPT_COMMAND="$PROMPT_COMMAND;update_terminal_cwd"
fi

add_to_path /usr/local/opt/gettext/bin
add_to_path /usr/local/opt/coreutils/libexec/gnubin
add_to_path ~/bin
add_to_path ~/.local/bin

source ~/.git-prompt.sh
source ~/.colors.sh

PS1="\[${COLOR_WHITE}\]\u@\h\[${COLOR_NC}\] \[${COLOR_YELLOW}\]"'$(path_tail)'"\[${COLOR_NC}\]\[${COLOR_LIGHT_BLUE}\]"'$(__git_ps1 " [%s]")'"\[${COLOR_NC}\] \$ "

export PAGER=less
export LESS="-R -S -X"

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
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

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

# save my sanity
alias ack-grep=ack
alias meld=/Applications/Meld.app/Contents/MacOS/meld
# Shell opts
alias ls="ls --color=auto"
# export GREP_OPTIONS='--color=auto'
export LS_OPTS='--color=auto'

if [ -x "$(command -v rbenv)" ]; then
  eval "$(rbenv init -)"
fi

if [ -x $HOME/bin/src-hilite-lesspipe.sh ]; then
    export LESSOPEN="|$HOME/bin/src-hilite-lesspipe.sh %s"
fi

## from http://stackoverflow.com/questions/3231804/in-bash-how-to-add-are-you-sure-y-n-to-any-command-or-alias
confirm () {
    # call with a prompt string or use a default
    read -r -p "${1:-Are you sure? [y/N]} " response
    case $response in
        [yY][eE][sS]|[yY])
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

export GIT_EDITOR=emacsclient
export UF_DOTENV=.env

if [ "$(uname)" == "Darwin" ]; then
  ulimit -n 65536 65536
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

