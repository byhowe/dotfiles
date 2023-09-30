;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Ömer Faruk Çavuş"
      user-mail-address "byhowe@proton.me")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

; Stretch cursor to the glyph width
(setq-default x-stretch-cursor t)

(setq undo-limit 80000000 ; Raise undo-limit to 80Mb
      ; By default while in insert all changes are one big blob. Be more
      ; granular
      evil-want-fine-undo t
      ; Nobody likes to loose work, I certainly don't
      auto-save-default t
      ; Unicode ellispis are nicer than "...", and also save /precious/ space
      truncate-string-ellipsis "…"
      ; It's nice to maintain a little margin
      scroll-margin 8)

;; My development directory
(setq dev-dir "~/Development")

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory (expand-file-name "Notes/Org" dev-dir))

;; Org agenda configuration
(setq org-agenda-files `(,(expand-file-name "Courses/METU/CurrentSemester/school.org" dev-dir))
      org-deadline-warning-days 30)

;; Org roam configuration
(setq org-roam-directory (expand-file-name "Notes/Roam" dev-dir)
      org-roam-dailies-directory "Daily"
      org-roam-dailies-capture-templates
      '(("d" "default" entry "* %?"
         :if-new
         (file+head "%<%Y-%m-%d>.org"
                    "#+title: %<%Y-%m-%d>\n#+author: %n\n"))))

;; Deft is a plugin for quickly writing notes and retrieving them later.
(setq deft-directory (expand-file-name "Notes/Deft" dev-dir)
      deft-extensions '("org" "txt" "md")
      deft-recursive 't)

;; Set transparency
(defun transparency (value)
  "Sets the transparency of the parent window, 0=transparent/100=opaque"
  (interactive "nEnter transparency value [0 - 100]: ")
  (set-frame-parameter (selected-frame) 'alpha value))
(transparency 95)

;; Do not ask to exit emacs
(setq confirm-kill-emacs nil)

;; Use bash as the shell
(setq shell-file-name (executable-find "bash"))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
