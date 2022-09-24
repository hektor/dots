[[ -f ~/.bashrc ]] && . ~/.bashrc

export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/ssh-agent.socket"
export QT_QPA_PLATFORMTHEME="qt6ct"

if [ -z "${DISPLAY}" ] && [ "${XDG_VTNR}" -eq 1 ]; then
    exec startx "$HOME/.config/X11/xinitrc"
fi
