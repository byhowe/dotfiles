;;; $DOOMDIR/latex.el -*- lexical-binding: t; -*-

;; Enable :ignore: tag so that when exporting an org document, the content is
;; kept but the heading is discarded.
(after! org
  (use-package! ox-extra
    :config
    (ox-extras-activate '(ignore-headlines))))

(setq org-latex-logfiles-extensions
      (quote ("lof" "lot" "tex~" "tex" "aux" "idx"
              "log" "out" "toc" "nav" "snm" "vrb" "dvi"
              "fdb_latexmk" "blg" "brf" "fls" "entoc"
              "ps" "spl" "bbl" "xmpi" "run.xml" "bcf"
              "acn" "acr" "alg" "glg" "gls" "ist")))

;; Set zathura as the latex viewer
(setq +latex-viewers '(zathura))

(after! ox-latex
  ;; Nicer checkboxes in latex
  (defun +org-export-latex-fancy-item-checkboxes (text backend info)
    (when (org-export-derived-backend-p backend 'latex)
      (replace-regexp-in-string
       "\\\\item\\[{$\\\\\\(\\w+\\)$}\\]"
       (lambda (fullmatch)
         (concat "\\\\item[" (pcase (substring fullmatch 9 -3) ; content of capture group
                               ("square"   "\\\\checkboxUnchecked")
                               ("boxminus" "\\\\checkboxTransitive")
                               ("boxtimes" "\\\\checkboxChecked")
                               (_ (substring fullmatch 9 -3))) "]"))
       text)))

  (add-to-list 'org-export-filter-item-functions
               '+org-export-latex-fancy-item-checkboxes)

  (setq org-latex-subtitle-format "\\newcommand{\\thesubtitle{\\\\\\medskip\n\\large %s}"
        org-latex-subtitle-separate 't)

  (add-to-list
   'org-latex-classes
   `("notes"
     ,(doom-file-read (expand-file-name "latex-class-notes.tex" doom-user-dir))
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
