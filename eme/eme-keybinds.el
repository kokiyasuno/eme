;;; eme-keybinds.el --- Keybindings for eme -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Koki Yasuno

;;; Commentary:

;; Default keybindings for Selection Mode and Insert Mode.
;;
;; Selection Mode keybindings:
;;
;; Movement (creates selection):
;; - f/b: forward/backward word (frequency-optimized)
;; - F/B: unassigned (reserved for Phase 3 jump)
;; - M-f/M-b: forward/backward sexp
;; - n/p: next/previous line (line selection when no anchor)
;; - N/P: forward/backward paragraph
;; - M-n/M-p: next/previous defun
;; - a/e: beginning/end of line
;; - A/E: beginning/end of sentence
;; - M-a/M-e: beginning/end of defun
;;
;; Selection expansion/contraction:
;; - v: expand selection (word → symbol → string → list → defun)
;; - V: contract selection (reverse of expand)
;;
;; Thing selection (t prefix):
;; - t i: select inner of enclosing delimiter
;; - t b: select bounds (including delimiters)
;; - t: repeat t to expand to outer delimiter
;; - t {delimiter}: explicit delimiter (e.g., t ( for parens)
;;
;; Anchor:
;; - .: toggle anchor (extend selection)
;;
;; Actions:
;; - d: delete (kill)
;; - D: delete (no kill-ring)
;; - w: copy
;; - y: yank (paste)
;; - c: change (delete + insert)
;; - r: replace with char
;; - x: exchange point and mark (flip selection)
;; - ;: toggle comment
;; - =: format/indent
;; - >/<: increase/decrease indent
;; - j: join lines
;; - o/O: open line below/above
;; - +: duplicate
;; - `: toggle case
;; - u/U: undo/redo
;;
;; Delimiter operations (m/M):
;; - m: select inner of nearest delimiter (or wrap if selection exists)
;; - M: select bounds (including delimiters)
;; After m selection:
;; - same delimiter: delete delimiter (2 strokes)
;; - different delimiter: change delimiter (2 strokes)
;; - m (repeat): expand to outer delimiter
;;
;; Search:
;; - /: search forward
;; - M-/: search regexp
;; - s: search next
;; - S: search previous
;;
;; Goto/View:
;; - g: goto line
;; - ,: xref pop (return from definition)
;; - C-v: scroll down (page forward)
;; - M-v: scroll up (page backward)
;; - l: recenter
;;
;; Macros:
;; - q: toggle macro record
;; - Q: play macro
;;
;; Register (prefix: '):
;; - ' {reg}: jump to register
;; - ' {reg} SPC: save position
;; - ' {reg} w: copy to register
;; - ' {reg} y: insert from register
;;
;; Mode transition:
;; - i: enter Insert Mode

;;; Code:

(require 'eme-core)
(require 'eme-selection)
(require 'eme-delimiter)
(require 'eme-actions)
(require 'eme-search)
(require 'eme-goto-view)
(require 'eme-macro-register)
(require 'eme-thing)
(require 'eme-expand)
(require 'eme-leader)

;;; g prefix keymap

(defvar eme-g-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" #'eme-sort-lines)
    (define-key map "R" #'eme-reverse-lines)
    map)
  "Keymap for g prefix commands.")

;;; Register prefix keymap

(defvar eme-register-read-map
  (let ((map (make-sparse-keymap)))
    ;; After reading register, wait for action
    ;; The actual bindings happen in eme-register-dispatch
    map)
  "Keymap for register reading.")

(defun eme-register-dispatch ()
  "Read a register and dispatch to appropriate action.
If followed by SPC, save position.
If followed by w, copy selection to register.
If followed by y, insert register content.
Otherwise, jump to register position."
  (interactive)
  (let ((register (read-char "Register: ")))
    (let ((action (read-char (format "' %c  [SPC=point w=copy y=insert]: " register))))
      (pcase action
        (?\s (eme-register-point-to register))
        (?w (eme-register-copy-to register))
        (?y (eme-register-insert register))
        (_ (eme-register-jump register))))))

;;; Selection Mode Keybindings

(defun eme--setup-selection-mode-keys ()
  "Set up all keybindings for Selection Mode."
  ;; Movement - word level (f/b = most frequent operation)
  (define-key eme-selection-mode-map "f" #'eme-forward-word)
  (define-key eme-selection-mode-map "b" #'eme-backward-word)

  ;; F/B unassigned (reserved for Phase 3 jump feature)
  ;; Character movement is available via Insert Mode C-f/C-b

  ;; Movement - sexp level
  (define-key eme-selection-mode-map (kbd "M-f") #'eme-forward-sexp)
  (define-key eme-selection-mode-map (kbd "M-b") #'eme-backward-sexp)

  ;; Movement - line level
  (define-key eme-selection-mode-map "n" #'eme-next-line)
  (define-key eme-selection-mode-map "p" #'eme-previous-line)

  ;; Movement - paragraph level
  (define-key eme-selection-mode-map "N" #'eme-forward-paragraph)
  (define-key eme-selection-mode-map "P" #'eme-backward-paragraph)

  ;; Movement - defun level
  (define-key eme-selection-mode-map (kbd "M-n") #'eme-next-defun)
  (define-key eme-selection-mode-map (kbd "M-p") #'eme-previous-defun)

  ;; Movement - line boundaries
  (define-key eme-selection-mode-map "a" #'eme-beginning-of-line)
  (define-key eme-selection-mode-map "e" #'eme-end-of-line)

  ;; Movement - sentence boundaries
  (define-key eme-selection-mode-map "A" #'eme-beginning-of-sentence)
  (define-key eme-selection-mode-map "E" #'eme-end-of-sentence)

  ;; Movement - defun boundaries
  (define-key eme-selection-mode-map (kbd "M-a") #'eme-beginning-of-defun)
  (define-key eme-selection-mode-map (kbd "M-e") #'eme-end-of-defun)

  ;; Anchor
  (define-key eme-selection-mode-map "." #'eme-toggle-anchor)

  ;; Actions
  (define-key eme-selection-mode-map "d" #'eme-delete)
  (define-key eme-selection-mode-map "D" #'eme-delete-no-save)
  (define-key eme-selection-mode-map "w" #'eme-copy)
  (define-key eme-selection-mode-map "y" #'eme-yank)
  (define-key eme-selection-mode-map "c" #'eme-change)
  (define-key eme-selection-mode-map "x" #'eme-exchange-point-and-mark)
  (define-key eme-selection-mode-map ";" #'eme-comment-toggle)
  (define-key eme-selection-mode-map "=" #'eme-format)
  (define-key eme-selection-mode-map ">" #'eme-indent-increase)
  (define-key eme-selection-mode-map "<" #'eme-indent-decrease)
  (define-key eme-selection-mode-map "j" #'eme-join-lines)
  (define-key eme-selection-mode-map "o" #'eme-open-line-below)
  (define-key eme-selection-mode-map "O" #'eme-open-line-above)
  (define-key eme-selection-mode-map "+" #'eme-duplicate)
  (define-key eme-selection-mode-map "`" #'eme-toggle-case)
  (define-key eme-selection-mode-map "u" #'eme-undo)
  (define-key eme-selection-mode-map "U" #'eme-redo)

  ;; Delimiter operations
  ;; m: inner selection (or wrap mode if selection exists)
  ;; M: bounds selection (including delimiters)
  (define-key eme-selection-mode-map "m" #'eme-delimiter-wrap-or-inner)
  (define-key eme-selection-mode-map "M" #'eme-delimiter-bounds)

  ;; Thing selection prefix
  (define-key eme-selection-mode-map "t" #'eme-thing-prefix)

  ;; Search
  (define-key eme-selection-mode-map "/" #'eme-search-forward)
  (define-key eme-selection-mode-map (kbd "M-/") #'eme-search-regexp)
  (define-key eme-selection-mode-map "s" #'eme-search-next)
  (define-key eme-selection-mode-map "S" #'eme-search-previous)
  (define-key eme-selection-mode-map "r" #'eme-replace-char)

  ;; Expand/Contract region
  (define-key eme-selection-mode-map "v" #'eme-expand-region)
  (define-key eme-selection-mode-map "V" #'eme-contract-region)

  ;; Goto/View
  (define-key eme-selection-mode-map "g" #'eme-goto-line)
  (define-key eme-selection-mode-map "," #'eme-xref-pop)
  (define-key eme-selection-mode-map (kbd "C-v") #'eme-scroll-up)
  (define-key eme-selection-mode-map (kbd "M-v") #'eme-scroll-down)
  (define-key eme-selection-mode-map "l" #'eme-recenter)

  ;; Macros
  (define-key eme-selection-mode-map "q" #'eme-macro-toggle-record)
  (define-key eme-selection-mode-map "Q" #'eme-macro-play)

  ;; Register prefix
  (define-key eme-selection-mode-map "'" #'eme-register-dispatch)

  ;; Mode transition
  (define-key eme-selection-mode-map "i" #'eme-enter-insert-mode)

  ;; Leader Key
  (define-key eme-selection-mode-map eme-leader-key #'eme-leader-start)

  ;; g prefix (for sort/reverse)
  ;; Note: simple goto-line is on g, but g s and g R need prefix
  ;; We'll use read-char dispatch for this
  )

;;; Insert Mode Leader Key

(define-key eme-insert-mode-map eme-leader-key-insert #'eme-leader-start)

;; Initialize keybindings
(eme--setup-selection-mode-keys)

(provide 'eme-keybinds)

;;; eme-keybinds.el ends here
