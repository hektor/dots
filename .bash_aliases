# Config quick-open

alias bashrc="nvim ~/.bashrc"
alias aliasrc="nvim ~/.aliasrc"
alias vimrc="nvim ~/.vimrc"

# Navigation

alias ..="cd .."
alias .2="cd ../.."
alias .3="cd ../../.."

# Colorize output

alias ls="ls --color=auto"
alias grep="grep --color=auto"
alias ip="ip --color"

# Listing

alias l="ls -1p"
alias ll="ls -lhp"
alias lt="ls -lhtp"
alias la="ls -lhap"
alias lta="ls -lhatp"
alias ldir="ls -dp" # list directories

# Safety

alias rm="rm -I --preserve-root"
alias mv="mv -iv"
alias cp="cp -iv"
alias ln="ln -i"
alias mkdir="mkdir -pv"
alias chown="chown --preserve-root"
alias chmod="chmod --preserve-root"
alias chgrp="chgrp --preserve-root"

# Clipboard

alias clip="xclip -sel clip"
alias srclip="clip -o | speedread -w 500"

# Other

alias cfg="/usr/bin/git --git-dir=/home/h/.cfg/ --work-tree=/home/h"
alias df="df -kTh"
alias fzfpac="pacman -Slq | fzf -m --preview 'pacman -Si {1}' | xargs -ro sudo pacman -S"
alias o="xdg-open"
alias path="echo -e ${PATH//:/\\n}" # Pretty print path variables

# Programs

alias feh="feh -B black --scale-down --auto-zoom"
alias fm='pcmanfm'
alias v="nvim"
alias zk="nvim +WikiIndex"
alias g='git'
alias lm='xbacklight -set'
alias py='python'
alias r5rs-repl='plt-r5rs --no-prim'
alias vi='vim'
alias xopp="xournalpp $1 &>/dev/null"
alias xev='xev | grep -A2 ButtonPress' # Ignore mouse movements

reboot_to_windows ()
{
  # Check if grub is installed by checking if the command exists, if it does
  # not, then assume that the system is using systemd-boot
  if grub-install --version &>/dev/null; then
    windows_title=$(grep -i windows /boot/grub/grub.cfg | cut -d "'" -f 2)
    sudo grub-reboot "$windows_title"
    echo "Grub set to reboot to Windows"
  else
    sudo bootctl set-oneshot windows.conf
    echo "Systemd set to reboot to Windows"
  fi
}
alias reboot-to-windows='reboot_to_windows'
