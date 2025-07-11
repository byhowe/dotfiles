;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap Evil (vim emulation)
(unless (package-installed-p 'evil)
  (package-refresh-contents)
  (package-install 'evil))

;; Enable Evil
(require 'evil)
(evil-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Set theme
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

;; Determine cache and data roots, falling back sensibly
(defconst my/xdg-cache-dir
  (or (getenv "XDG_CACHE_HOME")
      (expand-file-name ".cache/" (getenv "HOME")))
  "Where to put Emacs cache files.")
(defconst my/xdg-data-dir
  (or (getenv "XDG_DATA_HOME")
      (expand-file-name ".local/share/" (getenv "HOME")))
  "Where to put Emacs data files.")

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
