;; Determine cache and data roots, falling back sensibly
(defconst my/xdg-cache-dir
  (or (getenv "XDG_CACHE_HOME")
      (expand-file-name ".cache/" (getenv "HOME")))
  "Where to put Emacs cache files.")
(defconst my/xdg-data-dir
  (or (getenv "XDG_DATA_HOME")
      (expand-file-name ".local/share/" (getenv "HOME")))
  "Where to put Emacs data files.")

;; Bootstrap straight.el
(setq straight-base-dir my/xdg-data-dir)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
	"straight/repos/straight.el/bootstrap.el"
	(or (bound-and-true-p straight-base-dir)
	    user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Integration with use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(require 'use-package)

;; Vim emulation with Evil
(use-package evil
  :config
  (evil-mode 1))

;; Set theme
(setq modus-themes-italic-constructs t)
(load-theme 'modus-vivendi t)

;; turn off the startup splash screen
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t)

;; remove unnecessary gui elements
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode 0)

(global-display-line-numbers-mode 1) ;; modern line numbers
(global-hl-line-mode 1) ;; highlight the current line
(column-number-mode 1) ;; show column in mode line
(show-paren-mode 1) ;; blink matching parens instantly
(blink-cursor-mode 0) ;; disable blinking cursor

(setq-default show-trailing-whitespace t) ;; highlight trailing whitespace

;; Make sure those dirs exist
(dolist (dir (list
              (expand-file-name "emacs/backups/" my/xdg-data-dir)
              (expand-file-name "emacs/auto-save/" my/xdg-cache-dir)
	      (expand-file-name "emacs/auto-save-list/" my/xdg-data-dir)))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;; put all backup~ files in one place
(setq backup-directory-alist
      `((".*" . ,(expand-file-name "emacs/backups/" my/xdg-data-dir))))

;; don’t litter the working dir with #autosave# files
(setq auto-save-file-name-transforms
      `((".*/\\([^/]+\\)$" ;; capture the basename of the path
         ,(concat (expand-file-name "emacs/auto-save/" my/xdg-cache-dir)
                  "\\1") t)))

;; Move Emacs' crash-recovery files into XDG_DATA_HOME
(let ((save-list-dir (expand-file-name "emacs/auto-save-list/" my/xdg-data-dir)))
  (setq auto-save-list-file-prefix
	(concat (file-name-as-directory save-list-dir) ".saves-")))

;;; Separate Custom settings
(setq custom-file (expand-file-name "emacs/custom.el" my/xdg-data-dir))
(when (file-exists-p custom-file)
  (load custom-file))
