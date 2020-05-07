# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# load aliases dynamically
[ -f "$HOME/.aliasrc" ] && source "$HOME/.aliasrc"

# prompt
bold=$(tput bold)
reset=$(tput sgr0)

get_branch_name() {
  git symbolic-ref --quiet --short HEAD 2>/dev/null \
    || git rev-parse --short HEAD 2>/dev/null \
    || echo 'some branch'
}

get_git_info() {
  git rev-parse --is-inside-work-tree &>/dev/null || return
  echo -e "- ${1}$(get_branch_name)"
}

PS1='\[\033[38;5;244m\]\[${bold}\]$(get_git_info && echo "\n")\[${reset}\]\u \W $ '

# history
export HISTCONTROL=ignoreboth

# source fuzzy finder 
source /usr/share/fzf/*.bash

# vim-style keybindings
set -o vi
bind '"jj":"\e"'
