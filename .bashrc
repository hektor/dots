# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# load aliases dynamically
[ -f "$HOME/.aliasrc" ] && source "$HOME/.aliasrc"

PS1='\u@\h \W\$ '

# nvm
source /usr/share/nvm/init-nvm.sh

# history
export HISTCONTROL=ignoreboth

