# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# load aliases dynamically
[ -f "$HOME/.aliasrc" ] && source "$HOME/.aliasrc"

# add ~/.bin to PATH
export PATH=~/.bin:$PATH

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
  echo -e " ${1}$(get_branch_name)"
}

PS1='\[\033[38;5;244m\]\[${bold}\]$(get_git_info && echo "\n")\[${reset}\]\u \W ❭ '

# history
export HISTCONTROL=ignoreboth
export HISTSIZE=500000

# source fuzzy finder 
source /usr/share/fzf/*.bash

# vim-style keybindings
export EDITOR='vim'
set -o vi
bind '"jj":"\e"'

export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -t -g ""'
export FZF_COMPLETION_OPTS='-x'

# nvm
lazy_load_nvm() {
  unset -f nvm node npm npx
  export NVM_DIR="$HOME/.nvm"
  [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh" # This loads nvm
  [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
}

nvm() {
  lazy_load_nvm
  nvm $@
}

node() {
  lazy_load_nvm
  node $@
}

npm() {
  lazy_load_nvm
  npm $@
}

npx() {
  lazy_load_nvm
  npx $@
}
