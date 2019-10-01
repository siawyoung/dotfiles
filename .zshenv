# practically unlimited history
export HISTFILE=/Users/siawyoung/.zsh_history
export HISTSIZE=999999999
export SAVEHIST=$HISTSIZE
setopt EXTENDED_HISTORY
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_REDUCE_BLANKS

export EDITOR="emacsclient -nw"

alias rzsh="source ~/.zshrc"

# set all locales to UTF8
export LANG="en_US.UTF-8"
export LC_COLLATE="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"
export LC_MESSAGES="en_US.UTF-8"
export LC_MONETARY="en_US.UTF-8"
export LC_NUMERIC="en_US.UTF-8"
export LC_TIME="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"

export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

# i don't need any fancy themes, just give me pwd
export NEWLINE=$'\n'
export PROMPT="%/${NEWLINE}$ "

# python config
export PYENV_ROOT="$HOME/.pyenv"
export PYENV_VIRTUALENV_DISABLE_PROMPT=1
eval "$(pyenv init -)"

# Disable pip if you're not in a virtualenv
export PIP_REQUIRE_VIRTUALENV=true
# Define a new gpip function for pip install in non-virtualenv python
gpip() {
    PIP_REQUIRE_VIRTUALENV="" pip "$@"
}

export PYTHONSTARTUP="$HOME/.pythonstartup"

alias p2="pyenv shell 2.7.16"
alias p3="pyenv shell 3.7.0"

# google cloud platform config
# export PATH for google cloud sdk, uncomment if needed
export PATH="/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/bin:$PATH"

# golang config
export GOPATH="$HOME/github/go"
export GOBIN="$GOPATH/bin"
export PATH="$GOBIN:$PATH"
export PATH="$GOROOT/bin:$PATH"
export PATH="$HOME/.gotools:$PATH"
export PATH="$GOPATH/src/github.com/carousell/carousell-go/commons/tools:$PATH"

# for bins installed in alternative manners (not homebrew, etc)
export PATH="$PATH:$HOME/bin"

# for texinfo
export PATH="/usr/local/opt/texinfo/bin:$PATH"

# for source installed emacs
# ln -s $HOME/github/emacs/nextstep/Emacs.app/Contents/MacOS/Emacs /usr/local/bin/emacs
export PATH="$HOME/github/emacs/nextstep/Emacs.app/Contents/MacOS/bin:$PATH"

# rbenv config
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

# docker config
alias d="docker"
alias dc="docker-compose"
alias dm="docker-machine"

### Added by Zplugin's installer
source '/Users/siawyoung/.zplugin/bin/zplugin.zsh'
autoload -Uz _zplugin
(( ${+_comps} )) && _comps[zplugin]=_zplugin
### End of Zplugin's installer chunk

# fzf
zplugin ice from"gh-r" as"program"; zplugin load junegunn/fzf-bin
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# init direnv
# https://github.com/zdharma/zplugin/wiki/Direnv-explanation
zplugin ice from"gh-r" as"program" mv"direnv* -> direnv" atclone'./direnv hook zsh > zhook.zsh' atpull'%atclone' pick"direnv"
zplugin light direnv/direnv

zplugin ice "rupa/z" pick"z.sh"; zplugin light "rupa/z"
zplugin load changyuheng/fz

zplugin ice wait"0" lucid blockf
zplugin light zsh-users/zsh-completions

zplugin ice wait"0" lucid atinit"zpcompinit; zpcdreplay"
zplugin light zdharma/fast-syntax-highlighting
