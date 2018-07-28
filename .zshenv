# google cloud platform config
# export PATH for google cloud sdk, uncomment if needed
export PATH="/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/bin:$PATH"

# golang config
export GOPATH="$HOME/github/go"
export GOBIN="$GOPATH/bin"
export PATH="$GOBIN:$PATH"
export PATH="$HOME/.gotools:$PATH"

# pyenv config
export PYENV_ROOT="$HOME/.pyenv"
export PYENV_VIRTUALENV_DISABLE_PROMPT=1

# Disable pip if you're not in a virtualenv
export PIP_REQUIRE_VIRTUALENV=true
# Define a new gpip function for pip install in non-virtualenv python
gpip() {
   PIP_REQUIRE_VIRTUALENV="" pip "$@"
 }

# rbenv config
export PATH="$HOME/.rbenv/bin:$PATH"
