# shellcheck shell=bash
# shellcheck disable=SC1090
[[ -f ~/.bashrc ]] && . ~/.bashrc

export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/ssh-agent.socket"

if [ -z "${DISPLAY}" ] && [ "${XDG_VTNR}" -eq 1 ]; then
  exec startx "$HOME/.config/X11/xinitrc"
fi
