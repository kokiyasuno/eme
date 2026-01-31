;;; eme-goto-view.el --- Goto/View commands for eme -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Koki Yasuno

;;; Commentary:

;; Goto and View commands:
;; - g: goto line
;; - ,: xref pop (return from definition jump)
;; - v: scroll down (page forward)
;; - V: scroll up (page backward)
;; - l: recenter
;;
;; Standard Emacs keys preserved:
;; - M-<: beginning of buffer
;; - M->: end of buffer
;; - M-.: xref find definitions
;; - M-?: xref find references

;;; Code:

(require 'xref)
(require 'eme-core)

;;; Goto Commands

(defun eme-goto-line (line)
  "Go to LINE number.
If called with prefix argument, go directly to that line.
Otherwise, prompt for line number."
  (interactive
   (list (if current-prefix-arg
             (prefix-numeric-value current-prefix-arg)
           (read-number "Go to line: "))))
  (goto-char (point-min))
  (forward-line (1- line)))

;;; Xref Commands

(defun eme-xref-pop ()
  "Pop back to previous location in xref marker stack.
Use after M-. (go to definition) to return to previous location."
  (interactive)
  (xref-go-back))

;;; Scroll Commands

(defun eme-scroll-up ()
  "Scroll view up one page (show text below current view).
Error is suppressed at end of buffer."
  (interactive)
  (condition-case nil
      (scroll-up-command)
    (end-of-buffer nil)))

(defun eme-scroll-down ()
  "Scroll view down one page (show text above current view).
Error is suppressed at beginning of buffer."
  (interactive)
  (condition-case nil
      (scroll-down-command)
    (beginning-of-buffer nil)))

;;; Recenter Command

(defun eme-recenter ()
  "Recenter current line in window.
Cycles through center, top, bottom positions."
  (interactive)
  (recenter-top-bottom))

(provide 'eme-goto-view)

;;; eme-goto-view.el ends here
