#!/usr/bin/env bash

while [[ "$#" -gt 0 ]]; do case $1 in
  -m|--skip-mac) SKIP_MAC=1; shift;;
  -b|--skip-brew) SKIP_BREW=1; shift;;
  -d|--skip-doom) SKIP_DOOM=1; shift;;
  -p|--skip-python) SKIP_PYTHON=1; shift;;
  *) echo "Unknown parameter passed: $1"; exit 1;;
esac; shift; done


if [[ ! $SKIP_MAC == 1 ]] ; then
    # Remove all extraneous icons from the Dock
    defaults delete com.apple.dock persistent-apps; killall Dock

    # Move the dock position to the left
    defaults write com.apple.dock orientation -string left; killall Dock

    # Resize the dock
    defaults write com.apple.dock tilesize -int 36; killall Dock

    # Hide the dock
    defaults write com.apple.dock autohide -bool true && killall Dock
    defaults write com.apple.dock autohide-delay -float 0.2 && killall Dock
    defaults write com.apple.dock no-bouncing -bool TRUE && killall Dock

    # Enable three-finger drag (requires restart)
    defaults write com.apple.AppleMultitouchTrackpad TrackpadThreeFingerDrag -int 1
    defaults write com.apple.AppleMultitouchTrackpad TrackpadThreeFingerHorizSwipeGesture -int 1
    defaults write com.apple.AppleMultitouchTrackpad TrackpadThreeFingerVertSwipeGesture -int 1

    # Enable tap to click
    defaults write com.apple.AppleMultitouchTrackpad Clicking -int 1

    # Change trackpad tracking speed
    defaults write NSGlobalDomain com.apple.trackpad.scaling -float 1.5

    # Change key repeat rate
    defaults write -g InitialKeyRepeat -int 12
    defaults write -g KeyRepeat -int 2
fi

if [[ ! $SKIP_BREW == 1 ]] ; then
    # Install brew
    which -s brew
    if [[ $? != 0 ]] ; then
        /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    fi

    brew bundle --file=$HOME/dotfiles/mac/Brewfile

    ln -s $(brew --prefix)/opt/emacs-head/Emacs.app /Applications
    brew services start daviderestivo/emacs-head/emacs-head
fi

# Stow
stow doom zsh git
mkdir -p $HOME/.config
stow -t $HOME/.config karabiner

# Install zinit
sh -c "$(curl -fsSL https://raw.githubusercontent.com/zdharma/zinit/master/doc/install.sh)"

# Install some Pythons
if [[ ! $SKIP_PYTHON == 1 ]] ; then
pyenv install 2.7.17
pyenv install 3.8.0
pyenv global 3.8.0
fi

if [[ !$SKIP_DOOM == 1 ]] ; then
    git clone https://github.com/hlissner/doom-emacs ~/.config/emacs
    ~/.config/emacs/bin/doom install
    rm -rf ~/.emacs.d
    ln -s ~/.config/emacs ~/.emacs.d
fi

#####
##### From here on, things require sudo
#####

# Install some Nodes
# we need to do this since brew no longer chowns /usr/local
if [ ! -d "/usr/local/n" ] ; then
    sudo mkdir /usr/local/n
    sudo chown -R $(whoami) /usr/local/n
fi
n lts

if [ ! $SHELL == $(which zsh) ] ; then
    sudo sh -c "echo $(which zsh) >> /etc/shells"
    sudo chsh -s $(which zsh)
fi

if [[ ! $SKIP_BREW == 1 ]] ; then
    $(brew --prefix)/opt/fzf/install
fi
