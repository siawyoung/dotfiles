# practically unlimited history
export HISTFILE=/Users/siawyoung/.zsh_history
export HISTSIZE=999999999
export SAVEHIST=$HISTSIZE

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

# for bins installed in alternative manners (not homebrew, etc)
export PATH="$PATH:$HOME/bin"

# i don't need any fancy themes, just give me pwd
export NEWLINE=$'\n'
export PROMPT="%/${NEWLINE}$ "

### Added by Zplugin's installer
source '/Users/siawyoung/.zplugin/bin/zplugin.zsh'
autoload -Uz _zplugin
(( ${+_comps} )) && _comps[zplugin]=_zplugin
### End of Zplugin's installer chunk

# fzf
zplugin ice from"gh-r" as"program"; zplugin load junegunn/fzf-bin

# init direnv
# https://github.com/zdharma/zplugin/wiki/Direnv-explanation
zplugin ice from"gh-r" as"program" mv"direnv* -> direnv" atclone'./direnv hook zsh > zhook.zsh' atpull'%atclone' pick"direnv"
zplugin light direnv/direnv

zplugin ice "rupa/z" pick"z.sh"; zplugin light "rupa/z"
zplugin load changyuheng/fz
