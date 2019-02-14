#!/bin/bash

DOT_FILES=(
    .vim
    .vimrc
    .emacs.d
    .zshrc
)

for file in ${DOT_FILES[@]}
do
    ln -s $HOME/dotfiles/$file $HOME/$file
done
