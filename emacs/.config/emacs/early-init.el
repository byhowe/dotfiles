;; Built-in CPU porfiler
;(require 'profiler)
;(profiler-start 'cpu)
;(add-hook 'emacs-startup-hook
;          (lambda ()
;            (profiler-report)
;            (profiler-stop)))

;; Disable GC on startup
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
	  (lambda () (setq gc-cons-threshold (* 100 1024 1024)))) ; 100 mb

;; lsp optimizations

;; some language servers have responses in the 800k-3M range.
(setq read-process-output-max (* 5 1024 1024)) ; 5 mb

(setenv "LSP_USE_PLISTS" "true")

;; prevent package.el from loading packages
(setq package-enable-at-startup nil)

;; Disable unnecessary GUI elements early
(when (fboundp 'menu-bar-mode)   (menu-bar-mode   -1))
(when (fboundp 'tool-bar-mode)   (tool-bar-mode   -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; turn off the startup splash screen
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t)

;; load theme early to avoid flickering on startup
(setq modus-themes-italic-constructs t)
(load-theme 'modus-vivendi t)
