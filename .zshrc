#!/bin/zsh

# To profile shell startup, uncomment here and zprof at the bottom
# zmodload zsh/zprof

# Only run compinit once per day -- it's really slow.
autoload -Uz compinit
for dump in ~/.zcompdump(N.mh+24); do
    compinit
done
compinit -C

. ~/.zsh/env
. ~/.zsh/function
. ~/.zsh/zle
. ~/.zsh/style
. ~/.zsh/opts
. ~/.zsh/export
. ~/.zsh/alias

zle-line-init() {
    zle set-local-history 1
}
zle -N zle-line-init

# TODO: Find out why this isn't working.
zle-up-local() {
    zle set-local-history 1
    zle up-line-or-history
    zle set-local-history 0
}
zle -N zle-up-local
bindkey -M viins '^f' zle-up-local
bindkey -M vicmd '^f' zle-up-local

# fzf via Homebrew
if [ -e /usr/local/opt/fzf/shell/completion.zsh ]; then
    source /usr/local/opt/fzf/shell/key-bindings.zsh
    source /usr/local/opt/fzf/shell/completion.zsh
fi

if [[ "$TERM" != screen* ]]; then
    export TERM=rxvt
    ((tmux ls | grep -vq attached && tmux at) || tmux)
fi

fpath+=("$(brew --prefix)/share/zsh/site-functions")
autoload -U promptinit; promptinit
prompt pure

stty stop undef # Unmap ctrl-s

# zprof
