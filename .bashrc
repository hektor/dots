# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Load aliases dynamically
[ -f "$HOME/.bash_aliases" ] && source "$HOME/.bash_aliases"

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
export HISTCONTROL=ignoreboth:erasedups
export HISTSIZE=500000
export HISTIGNORE="clear:l: *"

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

export PATH=~/.nvm/versions/node/v14.16.0/bin:$PATH
export NVM_DIR="$HOME/.nvm"
[[ -s "$NVM_DIR/nvm.sh" ]] && source "$NVM_DIR/nvm.sh" --no-use

export XDG_SESSION_TYPE=X11

# Ellipsis when deep in directory
export PROMPT_DIRTRIM=2
