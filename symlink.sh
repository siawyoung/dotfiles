#!/bin/sh

ln -s ~/github/dotfiles/.direnvrc ~/
ln -s ~/github/dotfiles/.gitconfig ~/
ln -s ~/github/dotfiles/.gitignore_global ~/
ln -s ~/github/dotfiles/.emacs.d/ ~/
ln -s ~/github/dotfiles/.tmux.conf ~/
ln -s ~/github/dotfiles/.zshrc ~/
ln -s ~/github/dotfiles/.zshenv ~/
mkdir -p ~/.config/nvim
ln -s ~/github/dotfiles/.vimrc ~/.config/nvim/init.vim
