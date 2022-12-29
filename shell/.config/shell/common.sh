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
