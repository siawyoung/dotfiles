#!/usr/bin/env bash

# Mac-specific commands. Some of these require a restart/logout.

# Remove all extraneous icons from the Dock
defaults delete com.apple.dock persistent-apps; killall Dock

# Move the dock position to the left
defaults write com.apple.dock orientation -string left; killall Dock

# Resize the dock
defaults write com.apple.dock tilesize -int 36; killall Dock

# Enable three-finger drag (requires restart)
defaults write com.apple.AppleMultitouchTrackpad TrackpadThreeFingerDrag -int 1
defaults write com.apple.AppleMultitouchTrackpad TrackpadThreeFingerHorizSwipeGesture -int 1
defaults write com.apple.AppleMultitouchTrackpad TrackpadThreeFingerVertSwipeGesture -int 1

# Enable tap to click
defaults write com.apple.AppleMultitouchTrackpad Clicking -int 1

# Change trackpad tracking speed
defaults write NSGlobalDomain com.apple.trackpad.scaling -float 1.5
