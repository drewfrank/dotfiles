#!/bin/zsh

. ~/.zsh/env
. ~/.zsh/function
. ~/.zsh/zle
. ~/.zsh/style
. ~/.zsh/opts
. ~/.zsh/export
. ~/.zsh/alias
. ~/.zsh/prompt

if [[ "$TERM" != screen* ]]; then
    export TERM=rxvt
	screen -RR
fi
