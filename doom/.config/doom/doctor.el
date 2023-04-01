;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; $DOOMDIR/doctor.el

(unless (executable-find "typst")
  (warn! "Couldn't find typst binary"))
