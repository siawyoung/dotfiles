# google cloud platform config
# export PATH for google cloud sdk, uncomment if needed
export PATH="/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/bin:$PATH"

# golang config
export GOPATH="$HOME/github/go"
export GOROOT="/usr/local/opt/go/libexec"
export GOBIN="$GOPATH/bin"
export PATH="$GOBIN:$PATH"
export PATH="$GOROOT/bin:$PATH"
export PATH="$HOME/.gotools:$PATH"
export PATH="$GOPATH/src/github.com/carousell/carousell-go/commons/tools:$PATH"

# docker config
alias d="docker"
alias dc="docker-compose"
alias dm="docker-machine"

# python config
export PYTHONSTARTUP="$HOME/.pythonstartup"
