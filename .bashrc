# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Aliases {{{
# Load aliases dynamically
[ -f "$HOME/.bash_aliases" ] && source "$HOME/.bash_aliases"
# }}}

# Prompt {{{
get_branch_name() {
  git symbolic-ref --quiet --short HEAD 2>/dev/null \
    || git rev-parse --short HEAD 2>/dev/null \
    || echo 'some branch'
}
get_git_info() {
  git rev-parse --is-inside-work-tree &>/dev/null || return
  echo -e " $(get_branch_name)"
}

PS1="\u \w ❭\[$(tput sgr0)\] "

# Ellipsis when deep in directory
export PROMPT_DIRTRIM=2

export PS1
# }}}

# Path {{{
# Add ~/.bin to PATH
export PATH=~/.bin:$PATH
# }}}

# History {{{
export HISTCONTROL=ignoreboth:erasedups
export HISTSIZE=500000
# Omit `clear, ls...`; commands prepended with space
export HISTIGNORE="clear:l: *"
# }}}

# Man pages {{{
export MANWIDTH=120
# }}}

# Editor {{{
# Set vim as default editor
export EDITOR=nvim
# }}}

# X11 {{{
export XDG_SESSION_TYPE=X11
export XDG_CONFIG_HOME=$HOME/.config
# }}}

# Nix package manager {{{
# Add ~/.nix-profile/bin to PATH
export PATH=~/.nix-profile/bin:$PATH
# }}}

# FZF {{{
# Check if fzf is installed
if [ -f "/usr/bin/fzf" ]; then
  # Fuzzy finder setup
  export FZF_COMPLETION_TRIGGER='**'
  export FZF_DEFAULT_COMMAND='ag --hidden --skip-vcs-ignores -t -g ""'
  export FZF_DEFAULT_OPTS="
  --pointer='❭'
  --height 10%
  --color=fg:-1,bg:-1"
  export FZF_CTRL_T_COMMAND="${FZF_DEFAULT_COMMAND}"
  export FZF_CTRL_T_OPTS="--preview='bat {} | head -500'"
  source /usr/share/fzf/completion.bash
  source /usr/share/fzf/key-bindings.bash

  _fzf_setup_completion path vim
  _fzf_setup_completion path zathura
  _fzf_setup_completion path xournalpp
  _fzf_setup_completion path nvim
else
  echo "fzf not installed"
fi
# }}}

# Node {{{
# }}}

# Jupyter {{{
export JUPYTERLAB_DIR=$HOME/.local/share/jupyter/lab
# }}}

