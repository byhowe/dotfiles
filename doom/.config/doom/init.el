;;; $DOOMDIR/init.el -*- lexical-binding: t; -*-

(doom! :input
       ;;bidi
       ;;chinese
       ;;japanese
       ;;layout

       :completion
       (corfu +orderless +icons)
       (vertico +childframe +icons)
       ;;company
       ;;helm
       ;;ido
       ;;ivy

       :ui
       doom
       dashboard
       modeline
       (popup +defaults)
       hl-todo
       ophints
       nav-flash
       (vc-gutter +pretty)
       vi-tilde-fringe
       ligatures
       ;;deft
       ;;doom-quit
       ;;emoji
       ;;indent-guides
       ;;minimap
       ;;neotree
       ;;smooth-scroll
       ;;tabs
       ;;treemacs
       ;;unicode
       ;;window-select
       ;;workspaces
       ;;zen

       :editor
       (evil +everywhere)
       fold
       (format +lsp)
       multiple-cursors
       ;;file-templates
       ;;god
       ;;lispy
       ;;objed
       ;;parinfer
       ;;rotate-text
       ;;snippets
       ;;whitespace
       ;;word-wrap

       :emacs
       (dired +icons)
       electric
       undo
       ;;eww
       ;;ibuffer
       ;;tramp
       ;;vc

       :term
       (ghostel +everywhere)
       ;;eshell
       ;;shell
       ;;term
       ;;vterm

       :checkers
       (syntax +childframe +flymake +icons)
       (spell +hunspell +everywhere)
       ;;grammar

       :tools
       (lsp +eglot)
       tree-sitter
       lookup
       (eval +overlay)
       editorconfig
       magit
       direnv
       pdf
       biblio
       ;;ansible
       ;;collab
       ;;debugger
       ;;docker
       ;;ein
       ;;llm
       ;;make
       ;;pass
       ;;terraform
       ;;tmux
       ;;upload

       :os
       (:if (featurep :system 'macos) macos)
       ;;tty

       :lang
       (cc      +lsp +tree-sitter)
       (go      +lsp +tree-sitter)
       (haskell +lsp +tree-sitter)
       (python  +lsp +tree-sitter)
       (rust    +lsp +tree-sitter)
       (zig     +lsp +tree-sitter)

       (lua +tree-sitter)
       emacs-lisp
       sh

       data
       (json +lsp +tree-sitter)
       (yaml +tree-sitter)

       (latex +cdlatex +fold)
       (markdown +tree-sitter)
       (org +roam +pretty +noter +pandoc)

       :email
       ;;mu4e
       ;;notmuch
       ;;wanderlust

       :app
       ;;calendar
       ;;emms
       ;;everywhere
       ;;irc
       ;;rss

       :config
       ;;literate
       (default +bindings +smartparens))
