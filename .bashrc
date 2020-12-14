# from http://superuser.com/questions/39751/add-directory-to-path-if-its-not-already-there
if [ -x "$(command -v brew)" ]; then
    BREW_PREFIX=`brew --prefix`
else
    BREW_PREFIX=
fi

if [ -d "$HOME/.pyenv" ]; then
  export PYENV_ROOT="$HOME/.pyenv"
  export PATH="$PYENV_ROOT/bin:$PATH"
fi

eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

add_to_path ()
{
    if [[ "$PATH" =~ (^|:)"${1}"(:|$) ]]
    then
        return 0
    fi
    export PATH=${1}:$PATH
}

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

# NVM, so we don't break the system copy, such as homebrew
export NVM_DIR="$HOME/.nvm"
if [ -f /usr/local/opt/nvm/nvm.sh ]; then
    . "/usr/local/opt/nvm/nvm.sh"
fi

ulimit -n 65536 65536

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
