
set -gx RBENV_ROOT /usr/local/var/rbenv
. (rbenv init -|psub)

status --is-interactive; and . (pyenv init -|psub)
status --is-interactive; and . (pyenv virtualenv-init -|psub)

alias subl="reattach-to-user-namespace subl"

alias fconf="vim ~/.config/fish/config.fish"

alias s="subl ."
alias o="open ."

alias z="zeus start"
alias zs="zeus s"
alias zc="zeus c"
alias zr="zeus rake routes"

alias b="bundle"

alias stir="rm -f Gemfile.lock; b; rails c;"
alias cpd="bundle exec cap production deploy"

alias gpl="git log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"
alias gcb="git checkout -b"

alias rfish="source ~/.config/fish/config.fish"
alias push="git push"
alias pull="git pull"

alias d="docker"
alias dc="docker-compose"

set -gx LC_CTYPE en_US.UTF-8
set -gx LC_ALL en_US.UTF-8
