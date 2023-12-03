#!/bin/bash

echo "WARNING: This script is about to place symlinks in your home directory 
pointing to each file in this directory. If a file of the same name already 
exists in your home directory, a backup will be made with suffix \".bak\""
echo "";
read -p "Do you want to continue? (y/n) " RESP

if [ $RESP != "y" ]; then
    echo "Goodbye :)"
    exit;
fi

for file in $(ls -A); do
    if [ "$file" != "install.sh" -a "$file" != ".git" ]; then
        if [ -e "$HOME/$file" -a ! -L "$HOME/$file" ]; then
            echo "Creating backup: $HOME/$file => $HOME/$file.bak"
            mv $HOME/$file $HOME/$file.bak
        fi
        ln -sf $PWD/$file $HOME
        echo "INSTALLED ~/$file"
    fi
done
