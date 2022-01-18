# Config quick-open

alias bashrc="vim ~/.bashrc"
alias aliasrc="vim ~/.aliasrc"
alias vimrc="vim ~/.vimrc"

# Navigation

alias ..="cd .."
alias .2="cd ../.."
alias .3="cd ../../.."

# Colorize output

alias ls="ls --color=auto"
alias grep="grep --color=auto"
alias ip="ip --color"

# Listing

alias l="ls -1"
alias ll="ls -lh"
alias lt="ls -lht"
alias la="ls -lha"
alias lta="ls -lhat"
alias ld="ls -d */" # list directories

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

alias clip='xclip -sel clip'
alias srclip='clip -o | speedread -w 500'

# Other

alias df='df -kTh'
alias path='echo -e ${PATH//:/\\n}' # Pretty print path variables
alias cfg='/usr/bin/git --git-dir=/home/h/.cfg/ --work-tree=/home/h'
alias fzfpac="pacman -Slq | fzf -m --preview 'pacman -Si {1}' | xargs -ro sudo pacman -S"
alias zzz='systemctl suspend && exit'
alias o='xdg-open'
alias now='date +"%T"'
alias week='date +%V'

# Programs

alias g='git'
alias lm='xbacklight -set'
alias vi='vim'
alias py='python'
alias r5rs-repl='plt-r5rs --no-prim'
alias feh="feh -B black --scale-down --auto-zoom"

alias tlmgr='/usr/share/texmf-dist/scripts/texlive/tlmgr.pl --usermode'

reboot_to_windows ()
{
  windows_title=$(grep -i windows /boot/grub/grub.cfg | cut -d "'" -f 2)
    sudo grub-reboot "$windows_title" && sudo reboot
}
alias reboot-to-windows='reboot_to_windows'