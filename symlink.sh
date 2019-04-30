#!/bin/sh

ln -s ~/github/dotfiles/.direnvrc ~/
ln -s ~/github/dotfiles/.gitconfig ~/
ln -s ~/github/dotfiles/.gitignore_global ~/
ln -s ~/github/dotfiles/.emacs.d/ ~/
ln -s ~/github/dotfiles/.tmux.conf ~/
ln -s ~/github/dotfiles/.zshrc ~/
ln -s ~/github/dotfiles/.zshenv ~/
ln -s ~/github/dotfiles/.pythonstartup ~/
mkdir -p ~/.config/nvim
ln -s ~/github/dotfiles/.vimrc ~/.config/nvim/init.vim
ln -s ~/github/dotfiles/karabiner.json ~/.config/karabiner/karabiner.json
