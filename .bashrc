#
# ~/.bashrc
#

[ -z "$PS1" ] && return   # If not running interactively, don't do anything

# bash prompt styles
#PS1='[\[\e[37m\]\u\[\e[31m\]@\[\e[37m\]\h\[\e[0m\]:\[\e[33m\]\w\[\e[0m\]] \$ '
#PS1="\[\e[36;1m\][\[\e[34;1m\]\w\[\e[36;1m\]]: \[\e[0m\]"
#PS1="\[\e[0m\][\[\e[0m\]\w\[\e[0m\]]: \[\e[0m\]"
PS1="\[\e[0m\]\u \e[31m\]\w\[\e[0m\] "
#export PS1=" ${BLUE}[${NC} \u ${BLUE}]${NC} "
#export PS2="       ${NC}  :${BLUE} ] ${NC}"

# disable ^S/^Q flow control 
stty -ixon

# add ~/bin to PATH if it exists
if [ -d ~/bin ] ; then
   PATH=~/bin:"${PATH}"
fi

# bash options
shopt -s cmdhist          # save multi-line commands in history as single line
shopt -s checkwinsize     # update the values of LINES and COLUMNS after each command
shopt -s histappend       # append (not overwrite) the history file
shopt -s extglob          # enable egrep-style pattern matching
shopt -s cdspell          # autocorrects cd misspellings

# command history settings
alias hist='history | grep $1'    # search cmd history
export HISTFILE="$HOME/.bash_history_`hostname`"   # Hostname appended to bash history filename
export HISTSIZE=10000                                 # the bash history should save n commands
export HISTFILESIZE=${HISTSIZE}
export HISTCONTROL=erasedups  # erase duplicate cmds (also avail: ignorespace, ignoreboth, ignoredups)
# don't append the following to history: consecutive duplicate
# commands, ls, bg and fg, and exit
HISTIGNORE='\&:fg:bg:ls:pwd:cd ..:cd ~-:cd -:cd:jobs:set -x:ls -l:ls -l'
HISTIGNORE=${HISTIGNORE}':%1:%2:htop:top:mutt:sshfs*:ssh*:shutdown*'
export HISTIGNORE

export MAILPATH=${HOME}/mail/inbox"?You've got mail"'!'  # Mail prompt
#export MAILPATH=/var/spool/mail/$USER"?You've got mail"'!'  # Mail prompt
export BROWSER="links '%s' &"

# define editors and pagers
# worst-case choices
export EDITOR=/usr/bin/vi
export PAGER=/bin/more
# best-case choices
my_editor=vim
my_pager=less
type $my_editor >/dev/null 2>&1 && export EDITOR=$my_editor
type $my_pager >/dev/null 2>&1 && export PAGER=$my_pager


# bash completion
complete -cf sudo         # sudo tab-completion
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# general aliases
alias cp="cp -i"          # confirm before overwriting something
alias ls='ls --color=auto -F' # colourized list
alias ll='ls -lh'         # long list
alias la='ls -a'          # list all
alias f='find | grep'     # quick search
alias c='clear'           # clear screen
alias dir='ls -1'         # ms-style list
alias mem="free -m"       # free memory in MB
alias xmo='vim ~/.xmonad/xmonad.hs' # lazy
alias sv='sudo vim'       # lazy
alias sps='ps aux | grep -v grep | grep'  # search process
alias g="egrep --color=always"  # colourized grep
alias fixres="xrandr --size 1440x900"      # resets resolution
alias clam='clamscan --bell -i' # clamav scan a file
alias clamt='clamscan -r --bell -i ~/tmp' # clamav scan ~/tmp
alias gb='nh gnomebaker'  # launch gnomebaker as separate process
alias pacup='sudo pacman -Syu'
alias pacs='sudo pacman -Ss'
alias start='dbus-launch startx'
alias install='sudo pacman -Sy'
alias remove='sudo pacman -Rs'
alias yaoup='sudo yaourt -Syu --aur'
alias yaous='sudo yaourt -Ss'
alias yinstall='sudo yaourt -Sy'
alias yremove='sudo yaourt -Rs'
alias screenshot='cd ~/screenshots && scrot -cd3 desktop-%d-%m_%H:%M:%S.png -q 85 && cd'
alias cp="cp -v"
alias grep="grep --color=auto -n"
alias ls="ls -hF -a --color=auto"
alias mv="mv -v"
alias rm="rm -v"
alias halt="sudo shutdown -h now"
alias reboot="sudo reboot"
alias svim="sudo vim"
alias gitupdate="git-fetch origin && git-reset --hard origin/post-2.2"
alias netcfg="netcfg2"
alias irssilog="tail -f $HOME/.irssi_pipe | dzen2 -l 6 -tw 300" 
alias pacup="sudo pacman -Syu"
alias pac="sudo pacman -S"
#unalias -a               # uncomment to unalias everything 

# fuse/ssh aliases (vetted)
fu () {
  fusermount -u ${HOME}/fuse/$1
}


# bash functions

# nohup - run command detached from terminal and without output
# usage: nh <command>
nh() {
    nohup "$@" &>/dev/null &
}

# extract archives -- usage: ex <file>
extract () {
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2)   tar xjf $1        ;;
            *.tar.gz)    tar xzf $1     ;;
            *.bz2)       bunzip2 $1       ;;
            *.rar)       rar x $1     ;;
            *.gz)        gunzip $1     ;;
            *.tar)       tar xf $1        ;;
            *.tbz2)      tar xjf $1      ;;
            *.tgz)       tar xzf $1       ;;
            *.zip)       unzip $1     ;;
            *.Z)         uncompress $1  ;;
            *.7z)        7z x $1    ;;
            *)           echo "'$1' cannot be extracted via extract()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

# create archives -- usage: roll <foo.tar.gz> ./foo ./bar
roll () {
    FILE=$1
    case $FILE in
        *.tar.bz2) shift && tar cjf $FILE $* ;;
        *.tar.gz) shift && tar czf $FILE $* ;;
        *.tgz) shift && tar czf $FILE $* ;;
        *.zip) shift && zip $FILE $* ;;
        *.rar) shift && rar $FILE $* ;;
    esac
}

function calc() { echo "$*" | bc; }
function mktar() { tar czf "${1%%/}.tar.gz" "${1%%/}/"; }
function mkmine() { sudo chown -R ${USER} ${1:-.}; }

# search the vim reference manual for keyword
:h() {  vim --cmd ":silent help $@" --cmd "only"; }

# sanitize - set file/directory owner and permissions to normal values (644/755)
# Usage: sanitize <file>
sanitize() {
    chmod -R u=rwX,go=rX "$@"
    chown -R ${USER}.users "$@"
}

# GNU Screen Login greeting
if [ "$TERM" = "screen" -a ! "$SHOWED_SCREEN_MESSAGE" = "true" ]; then
  detached_screens=`screen -list | grep Detached`
  if [ ! -z "$detached_screens" ]; then
    echo "+---------------------------------------+"
    echo "| Detached screens are available:       |"
    echo "$detached_screens"
    echo "+---------------------------------------+"
  else
    echo "[ screen is activated ]"
  fi
  export SHOWED_SCREEN_MESSAGE="true"
fi

# linux console colours - ala Phrakture
if [ "$TERM" = "linux" ]; then
    echo -en "\e]P0222222" #black
    echo -en "\e]P8222222" #darkgrey
    echo -en "\e]P1803232" #darkred
    echo -en "\e]P9982b2b" #red
    echo -en "\e]P25b762f" #darkgreen
    echo -en "\e]PA89b83f" #green
    echo -en "\e]P3aa9943" #brown
    echo -en "\e]PBefef60" #yellow
    echo -en "\e]P4324c80" #darkblue
    echo -en "\e]PC2b4f98" #blue
    echo -en "\e]P5706c9a" #darkmagenta
    echo -en "\e]PD826ab1" #magenta
    echo -en "\e]P692b19e" #darkcyan
    echo -en "\e]PEa1cdcd" #cyan
    echo -en "\e]P7ffffff" #lightgrey
    echo -en "\e]PFdedede" #white
    clear #for background artifacting
fi
