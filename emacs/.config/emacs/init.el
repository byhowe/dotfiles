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
(setq straight-base-dir (expand-file-name "emacs" my/xdg-data-dir)
      straight-use-package-by-default t)
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
(require 'use-package)

;; Vim emulation with Evil
(use-package evil
  :config
  (evil-mode 1))

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

(use-package which-key
  :config (which-key-mode))

(use-package vertico
  :init
  (vertico-mode +1))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package projectile
  :init
  (projectile-mode 1)
  :bind (:map projectile-mode-map
	      ("s-p" . projectile-command-map)
	      ("C-c p" . projectile-command-map)))

(use-package magit)

;;(use-package company
;;  :config
;;  (global-company-mode 1))

;; LSP integration

;; the core package for lsp
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-c l")
  :hook ((rustic-mode . lsp-deferred)
	 (lsp-mode . lsp-enable-which-key-integration)))
  ;; :config
  ;; ;; Additional LSP performance optimizations
  ;; (setq lsp-completion-provider :none)  ; Use company-mode for completion
  ;; (setq lsp-headerline-breadcrumb-enable nil)  ; Disable breadcrumb for performance
  ;; (setq lsp-signature-auto-activate nil)  ; Disable signature help for performance
  ;; (setq lsp-signature-render-documentation nil)
  ;; (setq lsp-eldoc-hook nil)  ; Disable eldoc integration
  ;; (setq lsp-modeline-code-actions-enable nil)  ; Disable modeline code actions
  ;; (setq lsp-modeline-diagnostics-enable nil)  ; Disable modeline diagnostics
  ;; (setq lsp-log-io nil)  ; Disable logging for performance
  ;; (setq lsp-enable-file-watchers nil)  ; Disable file watchers for performance
  ;; (setq lsp-enable-folding nil)  ; Disable folding for performance
  ;; (setq lsp-enable-imenu nil)  ; Disable imenu integration for performance
  ;; (setq lsp-enable-snippet nil))  ; Disable snippet integration

;; lsp hovers, inline docs, sideline
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  ;; Enable sideline diagnostics
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-update-mode 'line)

  ;; Enable ui-doc
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-doc-use-childframe t)
  (setq lsp-ui-doc-position 'at-point)
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-mode-map
	      ("K" . lsp-ui-doc-show)))

;; Rust development environment
(use-package rustic
  :after lsp-mode
  :mode ("\\.rs\\'" . rustic-mode)
  :hook ((rustic-mode . lsp-deferred))
  :config
  (setq rustic-lsp-server 'rust-analyzer
        rustic-format-on-save t)
  :custom
  (rustic-cargo-use-last-stored-arguments t))

(use-package cargo
  :commands cargo-minor-mode
  :hook (rustic-mode . cargo-minor-mode))
