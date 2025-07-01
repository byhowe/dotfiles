export EDITOR=vim
export GOPATH="$HOME/go"

if status is-interactive
    # Commands to run in interactive sessions can go here
    fish_vi_key_bindings

    abbr l "exa -lah --group-directories-first"
    abbr rclone "mullvad-exclude rclone"
end

# Start X at login
if status is-login
    if test -z "$DISPLAY" -a "$XDG_VTNR" = 1
        # This is required to make optimus-manager work.
        sudo /usr/bin/prime-switch
        exec startx "$HOME/.config/xorg/xinitrc" -- -keeptty
    end
end

set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME ; set -gx PATH $HOME/.cabal/bin $HOME/.ghcup/bin $PATH # ghcup-env
set -gx PATH $HOME/go/bin $PATH
set -gx PATH $HOME/.local/bin $PATH
