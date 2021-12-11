# practically unlimited history
export HISTFILE=$HOME/.zsh_history
export HISTSIZE=999999999
export SAVEHIST=$HISTSIZE
setopt EXTENDED_HISTORY
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_REDUCE_BLANKS

# set all locales to UTF8
export LANG="en_US.UTF-8"
export LC_COLLATE="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"
export LC_MESSAGES="en_US.UTF-8"
export LC_MONETARY="en_US.UTF-8"
export LC_NUMERIC="en_US.UTF-8"
export LC_TIME="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"

# i don't need any fancy themes, just give me pwd
export NEWLINE=$'\n'
export PROMPT="%/${NEWLINE}$ "

# zsh's built in git tab completion
autoload -Uz compinit && compinit

# for bins installed in alternative manners (not homebrew, etc)
export PATH="$PATH:$HOME/bin"

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/shims:$PATH"
export PATH="$PYENV_ROOT/bin:$PATH"
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi

# java
export JAVA_HOME="/usr/lib/jvm/java-11-openjdk-amd64/bin/java"
export PATH="$PATH:$JAVA_HOME"

# node management
export N_PREFIX="$HOME/n"; [[ :$PATH: == *":$N_PREFIX/bin:"* ]] || PATH+=":$N_PREFIX/bin"  # Added by n-install (see http://git.io/n-install-repo).

# Load all additional zsh layers (for work, etc)
# don't throw error if no glob matches found
setopt NULL_GLOB
for x in $HOME/.zshrc_*;
    do if [[ $x ]]; then
        source $x;
    fi
done
unsetopt NULL_GLOB


eval "$(direnv hook zsh)"

eval "$(rbenv init -)"
