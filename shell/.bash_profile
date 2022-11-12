# This file is sourced by bash for login shells. The following line
# runs your .bashrc and is recommended by the bash info pages.
if [[ -f ~/.bashrc ]]; then
  . ~/.bashrc
elif [[ -f ~/.config/shell/common.sh ]]; then
  . ~/.config/shell/common.sh
else
  export XINITRC="$HOME/.config/xorg/xinitrc"
fi

# Autostart X at login.
if [ -z "${DISPLAY}" ] && [ "${XDG_VTNR}" -eq 1 ]; then
  exec startx "$XINITRC"
fi
