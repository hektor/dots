# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Load aliases dynamically
[ -f "$HOME/.aliasrc" ] && source "$HOME/.aliasrc"

# Add ~/.bin to PATH
export PATH=~/.bin:$PATH

# Prompt
get_branch_name() {
  git symbolic-ref --quiet --short HEAD 2>/dev/null \
    || git rev-parse --short HEAD 2>/dev/null \
    || echo 'some branch'
}
get_git_info() {
  git rev-parse --is-inside-work-tree &>/dev/null || return
  echo -e " $(get_branch_name)"
}


export PS1="\u \w ❭\[$(tput sgr0)\] "

# History
export HISTCONTROL=ignoreboth
export HISTSIZE=500000

# Readable man pages
export MANWIDTH=120

# Vim
export EDITOR='vim'

# Fuzzy finder setup
source /usr/share/fzf/*.bash
source /usr/share/fzf/key-bindings.bash
export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -t -g ""'
export FZF_DEFAULT_OPTS='--height 38% --reverse --border --prompt="❭ " --pointer="❭"'
export FZF_COMPLETION_OPTS='-x'

# Nvm
lazy_load_nvm() {
  unset -f nvm node npm npx
  export NVM_DIR="$HOME/.nvm"
  [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"
  [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
}
nvm() { lazy_load_nvm nvm $@; }
node() { lazy_load_nvm node $@; }
npm() { lazy_load_nvm npm $@; }
npx() { lazy_load_nvm npx $@; }
