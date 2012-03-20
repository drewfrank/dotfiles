#!/bin/bash

for file in $(ls -A); do
    if [ "$file" != "install.sh" ]; then
        if [ -e "$HOME/$file" -a ! -L "$HOME/$file" ]; then
            echo "Creating backup: $HOME/$file => $HOME/$file.bak"
            mv $HOME/$file $HOME/$file.bak
        fi
        ln -sfT $PWD/$file $HOME/$file
        echo "INSTALLED ~/$file"
    fi
done
