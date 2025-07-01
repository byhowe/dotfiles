# This file is sourced by all *interactive* bash shells on startup,
# including some apparently interactive shells such as scp and rcp
# that can't tolerate any output.  So make sure this doesn't display
# anything or bad things will happen!

# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]] ; then
  # Shell is non-interactive.  Be done now!
  return
fi

# Bash won't get SIGWINCH if another process is in the foreground.
# Enable checkwinsize so that bash will check the terminal size when
# it regains control.  #65623
# http://cnswww.cns.cwru.edu/~chet/bash/FAQ (E11)
shopt -s checkwinsize

# Disable completion when the input buffer is empty.  i.e. Hitting tab
# and waiting a long time for bash to expand all of $PATH.
shopt -s no_empty_cmd_completion

# Enable history appending instead of overwriting when exiting.  #139609
shopt -s histappend

# Save each command to the history file as it's executed.  #517342
# This does mean sessions get interleaved when reading later on, but this
# way the history is always up to date.  History is not synced across live
# sessions though; that is what `history -n` does.
# Disabled by default due to concerns related to system recovery when $HOME
# is under duress, or lives somewhere flaky (like NFS).  Constantly syncing
# the history will halt the shell prompt until it's finished.
#PROMPT_COMMAND='history -a'

# Change the window title of X terminals 
case ${TERM} in
  [aEkx]term*|rxvt*|gnome*|konsole*|interix|tmux*)
    PS1='\[\033]0;\u@\h:\w\007\]'
    ;;
  screen*)
    PS1='\[\033_\u@\h:\w\033\\\]'
    ;;
  *)
    unset PS1
    ;;
esac

# Set colorful PS1 only on colorful terminals.
# dircolors --print-database uses its own built-in database
# instead of using /etc/DIR_COLORS.  Try to use the external file
# first to take advantage of user additions.
# We run dircolors directly due to its changes in file syntax and
# terminal name patching.
use_color=false
if type -P dircolors >/dev/null ; then
  # Enable colors for ls, etc.  Prefer ~/.dir_colors #64489
  LS_COLORS=
  if [[ -f ~/.dir_colors ]] ; then
    eval "$(dircolors -b ~/.dir_colors)"
  elif [[ -f /etc/DIR_COLORS ]] ; then
    eval "$(dircolors -b /etc/DIR_COLORS)"
  else
    eval "$(dircolors -b)"
  fi
  # Note: We always evaluate the LS_COLORS setting even when it's the
  # default.  If it isn't set, then `ls` will only colorize by default
  # based on file attributes and ignore extensions (even the compiled
  # in defaults of dircolors). #583814
  if [[ -n ${LS_COLORS:+set} ]] ; then
    use_color=true
  else
    # Delete it if it's empty as it's useless in that case.
    unset LS_COLORS
  fi
else
  # Some systems (e.g. BSD & embedded) don't typically come with
  # dircolors so we need to hardcode some terminals in here.
  case ${TERM} in
  [aEkx]term*|rxvt*|gnome*|konsole*|screen|tmux|cons25|*color) use_color=true;;
  esac
fi

if ${use_color} ; then
  if [[ ${EUID} == 0 ]] ; then
    PS1+='\[\033[01;31m\]\h\[\033[01;34m\] \w \$\[\033[00m\] '
  else
    PS1+='\[\033[01;32m\]\u@\h\[\033[01;34m\] \w \$\[\033[00m\] '
  fi

  #BSD#@export CLICOLOR=1
  #GNU#@alias ls='ls --color=auto'
  alias grep='grep --colour=auto'
else
  # show root@ when we don't have colors
  PS1+='\u@\h \w \$ '
fi

for sh in /etc/bash/bashrc.d/* ; do
  [[ -r ${sh} ]] && source "${sh}"
done

# Try to keep environment pollution down, EPA loves us.
unset use_color sh


export GOPATH="$HOME/go"
export PATH="$GOPATH/bin:$HOME/.local/bin:$PATH"

export RUST_TOOLCHAIN="$HOME/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu"
export PATH="$RUST_TOOLCHAIN/bin:$PATH"

export ANDROID_HOME="$HOME/Android/Sdk"
export PATH="$ANDROID_HOME/tools/bin:$PATH"

# XDG
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"

# Common programs
export VISUAL="nvim"
export EDITOR="nvim"
export TERMINAL="alacritty"
export BROWSER="firefox-developer-edition"

# Declutter ~
export LESSHISTFILE="-"
export WGETRC="${XDG_CONFIG_HOME:-$HOME/.config}/wgetrc"
export XINITRC="${XDG_CONFIG_HOME:-$HOME/.config}/xorg/xinitrc"
export XINITRC_STEAM="${XDG_CONFIG_HOME:-$HOME/.config}/xorg/xinitrc_steam"
export XINITRC_PLASMA="${XDG_CONFIG_HOME:-$HOME/.config}/xorg/xinitrc_plasma"

# IM
export GTK_IM_MODULE="fcitx"
export QT_IM_MODULE="fcitx"
export SDL_IM_MODULE="fcitx"
export XMODIFIERS="@im=fcitx"

# ls
alias exa="exa --color=always --group-directories-first"
alias ls="exa"
alias la="ls -a"
alias ll="ls -lah"
alias l="ls -lah"

alias ydl-audio="youtube-dl --audio-quality 0 --extract-audio --audio-format flac"
alias ydl-video="youtube-dl -f 'bestvideo+bestaudio' --merge-output-format mp4 --audio-quality 0"

alias dotfiles="git --git-dir=${HOME}/Development/Configurations/dotfiles/.git --work-tree=${HOME}"

alias rclone="mullvad-exclude rclone"

# usage: format <language> <dir>
format ()
{
  dir="${2:-.}"
  if [ -d $dir ]; then
    case $1 in
      rust) find $dir -type d -name target -prune -o -type f -name '*.rs' -print -exec rustfmt {} \; ;;
      haskell) find $dir -name '.stack-work' -prune -o -type f -name '*.hs' -print -exec hindent {} \; -exec stylish-haskell -i {} \; && hlint . ;;
    esac
  else
    echo "'$dir' is not a valid directory"
  fi
}

# usage: ex <file>
ex ()
{
  if [ -f $1 ]; then
    case $1 in
      *.tar.bz2) tar xjf $1   ;;
      *.tar.gz)  tar xzf $1   ;;
      *.bz2)     bunzip2 $1   ;;
      *.rar)     unrar x $1   ;;
      *.gz)      gunzip $1    ;;
      *.tar)     tar xf $1    ;;
      *.tbz2)    tar xjf $1   ;;
      *.tgz)     tar xzf $1   ;;
      *.zip)     unzip $1     ;;
      *.Z)       uncompress $1;;
      *.7z)      7z x $1      ;;
      *.deb)     ar x $1      ;;
      *.tar.xz)  tar xf $1    ;;
      *.tar.zst) unzstd $1    ;;
      *)         echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

test-window-manager ()
{
  startx "$XINITRC" -- /usr/bin/Xephyr -br -ac -noreset -screen "${1:-1600x900}" :1
}

source ~/.config/broot/launcher/bash/br
[ -f "/home/charlie/.ghcup/env" ] && source "/home/charlie/.ghcup/env" # ghcup-env

