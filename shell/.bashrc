#!/bin/bash
# This file is sourced by all *interactive* bash shells on startup,
# including some apparently interactive shells such as scp and rcp
# that can't tolerate any output. So make sure this doesn't display
# anything or bad things will happen!

# Test for an interactive shell. There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
[[ $- != *i* ]] && return

# Bash won't get SIGWINCH if another process is in the foreground.
# Enable checkwinsize so that bash will check the terminal size when
# it regains control.
shopt -s checkwinsize

# Disable completion when the input buffer is empty. i.e. Hitting tab
# and waiting a long time for bash to expand all of $PATH.
shopt -s no_empty_cmd_completion

# Enable history appending instead of overwriting when exiting.
shopt -s histappend

# Change the window title of X terminals.
case ${TERM} in
  [aEkx]term*|rxvt*|gnome*|konsole*|interix|tmux*)
    PS1='\[\033]0;\u@\h:\w\007\]'
    ;;
  screen*)
    PS1='\[\033k\u@\h:\w\033\\\]'
    ;;
  *)
    unset PS1
    ;;
esac

# Set colorful PS1 only on colorful terminals.
# dircolors --print-database uses its own built-in database
# instead of using /etc/DIRCOLORS. Try to use the external file
# first to take advantage of user additions.
# We run dircolors directly due to its changes in file syntax and
# terminal name patching.
use_color=false
if type -P dircolors >/dev/null ; then
  # Enable colors for ls, etc. Prefer ~/.dir_colors
  LS_COLORS=
  if [[ -f ~/.dir_colors ]] ; then
    eval "$(dircolors -b ~/.dir_colors)"
  elif [[ -f /etc/DIRCOLORS ]] ; then
    eval "$(dircolors -b /etc/DIRCOLORS)"
  else
    eval "$(dircolors -b)"
  fi
  # Note: We always evaluate the LS_COLORS setting even when it's the
  # default. If it isn't set, then `ls` will only colorize by default
  # based on file attributes and ignore extensions (even the compiled
  # in the defaults of dircolors).
  if [[ -n "${LS_COLORS:+set}" ]] ; then
    use_color=true
  else
    # Delete it if it's empty as it's useless in that case.
    unset LS_COLORS
  fi
else
  # Some systems (e.g. BSD & embedded) don't typically come with
  # dircolors so we need to hardcode some terminals in here.
  case ${TERM} in
  [aEkx]term*|rxvt*|gnome*|konsole*|screen|tmux|cons25|*color) use_color=true ;;
  esac
fi

if ${use_color} ; then
  if [[ ${EUID} == 0 ]] ; then
    PS1+='\[\033[01;31m\]\h\[\033[01;34m\] \w \$\[\033[00m\] '
  else
    PS1+='\[\033[01;32m\]\u@\h\[\033[01;34m\] \w \$\[\033[00m\] '
  fi
else
  # show root@ when we don't have colors
  PS1+='\u@\h \w \$ '
fi

# Enable bash completion in interactive shells
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  fi
fi

# Search the repos when command not found.
if [ -x /usr/bin/pkgfile ]; then
  function command_not_found_handle {
    # Check because pkgfile could've been removed in the meantime.
    if [ -x /usr/bin/pkgfile ]; then
      printf "The command you are looking for could be in one of these packages:\n"
      /usr/bin/pkgfile -sb -- "$1"
      return $?
    else
      printf "%s: command not found\n" "$1" >&2
      return 127
    fi
  }
fi

# Try to keep environment pollution down, EPA loves us.
unset use_color

. ~/.config/shell/common.sh
