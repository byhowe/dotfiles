;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Ömer Faruk Çavuş"
      user-mail-address "37745048+byhowe@users.noreply.github.com")

;; Tip: 'M-x describe-font'
(setq doom-font                 (font-spec :family "Hack Nerd Font"    :size 13 :weight 'regular)
      doom-variable-pitch-font  (font-spec :family "Liberation Sans"   :size 13)
      doom-serif-font           (font-spec :family "Liberation Serif"  :size 13))

(setq doom-theme 'doom-tokyo-night
      display-line-numbers-type 'relative)

(setq ispell-dictionary "en_US")
(remove-hook 'text-mode-hook #'spell-fu-mode)

;; Disable line numbers on the dashboard page and in org documents
(add-hook '+doom-dashboard-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))

(setq my/directory "~/Development/"
      org-directory (expand-file-name "Org" my/directory)
      org-roam-directory org-directory
      org-roam-dailies-directory "Daily/")

(setq org-deadline-warning-days 30
      org-list-allow-alphabetical t)

(defun my/get-template (filename)
  (with-temp-buffer
    (insert-file-contents (expand-file-name filename doom-user-dir))
    (buffer-string)))

(setq org-roam-capture-templates
      '(("d" "default" plain "%?"
         :target (file+head "%<%Y%m%d%H%m%S>-${slug}.org" ,(my/get-template "templates/roam-node.org"))
         :unnarrowed t)))

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry "* %?"
         :target (file+head "%<%Y-%m-%d>.org" ,(my/get-template "templates/roam-daily.org")))))

(setq-default show-trailing-whitespace t
              x-stretch-cursor t) ; Stretch cursor to glyph width

(add-hook 'ghostel-mode-hook (lambda () (setq show-trailing-whitespace nil)))

(setq undo-limit 80000000    ; Raise undo-limit to 80Mb
      evil-want-fine-undo t  ; Granular undo
      scroll-margin 16)

(defadvice! +my/center-screen-after-movement-a (&rest _)
  "Center the screen after specific movement commands."
  :after '(evil-ex-search-next
           evil-ex-search-previous
           evil-search-next
           evil-search-previous
           better-jumper-jump-forward
           better-jumper-jump-backward
           org-next-visible-heading
           org-previous-visible-heading
           org-forward-heading-same-level
           org-backward-heading-same-level)
  (evil-scroll-line-to-center nil))

(setq confirm-kill-emacs nil)

(with-eval-after-load 'ghostel
  (setq ghostel-module-auto-install 'compile))
