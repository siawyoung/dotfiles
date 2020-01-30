#!/usr/bin/env bash

# Remove all extraneous icons from the Dock
defaults delete com.apple.dock persistent-apps; killall Dock

# Move the dock position to the left
defaults write com.apple.dock orientation -string left; killall Dock

# Resize the dock
defaults write com.apple.dock tilesize -int 36; killall Dock

# Hide the dock
defaults write com.apple.dock autohide -bool true && killall Dock
defaults write com.apple.dock autohide-delay -float 2 && killall Dock
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

# Install brew
which -s brew
if [[ $? != 0 ]] ; then
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

brew bundle --file=$HOME/dotfiles/mac/Brewfile

# Stow
stow doom zsh git karabiner

# Install zinit
sh -c "$(curl -fsSL https://raw.githubusercontent.com/zdharma/zinit/master/doc/install.sh)"

# Install some Pythons
pyenv install 3.8.0

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

# put casks which prompt for password at the end, not sure how to remove
# the need for passwords entirely since sudo brew isn't supported anymore
brew bundle --file=$HOME/dotfiles/mac/Brewfile-sudo
