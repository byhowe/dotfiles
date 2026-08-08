;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Ömer Faruk Çavuş"
      user-mail-address "37745048+byhowe@users.noreply.github.com")

;; Tip: 'M-x describe-font'
(setq doom-font                 (font-spec :family "Hack Nerd Font"    :size 13 :weight 'regular)
      doom-variable-pitch-font  (font-spec :family "Liberation Sans"   :size 13)
      doom-serif-font           (font-spec :family "Liberation Serif"  :size 13))

(setq doom-theme 'doom-one
      display-line-numbers-type 'relative)

(setq-default show-trailing-whitespace t
              x-stretch-cursor t) ; Stretch cursor to glyph width

(setq undo-limit 80000000    ; Raise undo-limit to 80Mb
      evil-want-fine-undo t  ; Granular undo
      scroll-margin 16)

(setq confirm-kill-emacs nil)

(with-eval-after-load 'ghostel
  (setq ghostel-module-auto-install 'compile))
