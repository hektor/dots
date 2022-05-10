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
# Omit `clear, ls...`; commands prepended with space
export HISTIGNORE="clear:l: *"

# Man pages
export MANWIDTH=120

# Vim
export EDITOR='vim'

# Fuzzy finder setup
export FZF_COMPLETION_TRIGGER='**'
export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -t -g ""'
export FZF_DEFAULT_OPTS="
--pointer="❭"
--height 10%
--color=fg:-1,bg:-1"
export FZF_CTRL_T_COMMAND="${FZF_DEFAULT_COMMAND}"
export FZF_CTRL_T_OPTS="--preview='bat {} | head -500'"
source /usr/share/fzf/completion.bash
source /usr/share/fzf/key-bindings.bash
_fzf_setup_completion path vim
_fzf_setup_completion path zathura
_fzf_setup_completion path xournalpp

# Nvm
export PATH=~/.nvm/versions/node/v14.16.0/bin:$PATH
export NVM_DIR="$HOME/.nvm"
[[ -s "$NVM_DIR/nvm.sh" ]] && source "$NVM_DIR/nvm.sh" --no-use

# X11
export XDG_SESSION_TYPE=X11

# Jupyter
export JUPYTERLAB_DIR=$HOME/.local/share/jupyter/lab

# Ellipsis when deep in directory
export PROMPT_DIRTRIM=2
