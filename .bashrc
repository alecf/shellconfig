# from http://superuser.com/questions/39751/add-directory-to-path-if-its-not-already-there
if [ -x "$(command -v brew)" ]; then
    BREW_PREFIX=`brew --prefix`
else
    BREW_PREFIX=
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

add_to_path /usr/local/opt/coreutils/libexec/gnubin
add_to_path ~/bin

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
# Shell opts
alias ls="ls --color=auto"
export GREP_OPTIONS='--color=auto'
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

_eslintd() {
    git diff --name-only --relative $* | grep '\.js$' | xargs -t ./node_modules/.bin/eslint --fix
}
alias gbage='for k in `git branch | perl -pe s/^..//`; do echo -e `git show --pretty=format:"%Cgreen%ci %Cblue%cr%Creset" $k -- | head -n 1`\\t$k; done | sort -r'
alias eslintd=_eslintd
alias dc=docker-compose
alias dclf='docker-compose logs --tail=20 -f'
alias dc-lint-watch='docker-compose exec frontend ./node_modules/.bin/watch "time ./node_modules/.bin/eslint --color --cache uf" uf'
alias dc-lint='docker-compose exec frontend ./node_modules/.bin/eslint --color --cache uf'
export GIT_EDITOR=emacsclient
export UF_DOTENV=.env

# NVM, so we don't break the system copy, such as homebrew
export NVM_DIR="$HOME/.nvm"
if [ -x /usr/local/opt/nvm/nvm.sh ]; then
    . "/usr/local/opt/nvm/nvm.sh"
fi

ulimit -n 65536 65536
