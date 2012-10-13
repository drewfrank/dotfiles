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
    (tmux ls | grep -vq attached && tmux at) || tmux
fi

stty stop undef # Unmap ctrl-s
