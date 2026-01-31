;;; eme-macro-register.el --- Macro and register commands for eme -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Koki Yasuno

;;; Commentary:

;; Macro commands:
;; - q: toggle macro recording (start/stop)
;; - Q: play last macro
;;
;; Register commands (prefix: '):
;; - ' {reg}: jump to register position
;; - ' {reg} SPC: save position to register
;; - ' {reg} w: save selection text to register
;; - ' {reg} y: insert register text

;;; Code:

(require 'kmacro)
(require 'register)
(require 'eme-core)

;;; Macro Commands

(defun eme-macro-toggle-record ()
  "Toggle macro recording.
If not recording, start recording.
If recording, stop recording."
  (interactive)
  (if defining-kbd-macro
      (progn
        (end-kbd-macro)
        (message "Macro recording stopped"))
    (start-kbd-macro nil)
    (message "Macro recording started")))

(defun eme-macro-play (count)
  "Play last recorded macro COUNT times.
With prefix argument, play that many times."
  (interactive "p")
  (if last-kbd-macro
      (call-last-kbd-macro count)
    (message "No macro recorded")))

;;; Register Commands

(defvar eme-register-prefix-map
  (let ((map (make-sparse-keymap)))
    map)
  "Prefix keymap for register operations.")

(defun eme-register-jump (register)
  "Jump to position stored in REGISTER.
If REGISTER contains a marker or window configuration, jump to it."
  (interactive
   (list (register-read-with-preview "Jump to register: ")))
  (let ((val (get-register register)))
    (cond
     ((not val)
      (message "Register '%c' is empty" register))
     ((markerp val)
      (switch-to-buffer (marker-buffer val))
      (goto-char val))
     ((and (consp val) (frame-configuration-p (car val)))
      (set-frame-configuration (car val) (not current-prefix-arg)))
     ((and (consp val) (window-configuration-p (car val)))
      (set-window-configuration (car val))
      (goto-char (cadr val)))
     ((and (consp val) (eq (car val) 'file))
      (find-file (cdr val)))
     ((and (consp val) (eq (car val) 'file-query))
      (or (find-buffer-visiting (nth 1 val))
          (y-or-n-p (format "Visit file %s? " (nth 1 val)))
          (error "Not visiting file"))
      (find-file (nth 1 val))
      (goto-char (nth 2 val)))
     (t
      (message "Register '%c' contains text, not a position" register)))))

(defun eme-register-point-to (register)
  "Store current position in REGISTER."
  (interactive
   (list (register-read-with-preview "Save position to register: ")))
  (point-to-register register)
  (message "Position saved to register '%c'" register))

(defun eme-register-copy-to (register)
  "Copy selection to REGISTER.
If no eme-selection is active, copy current line."
  (interactive
   (list (register-read-with-preview "Copy to register: ")))
  (if (eme-selection-active-p)
      (let ((bounds (eme-selection-bounds)))
        (copy-to-register register (car bounds) (cdr bounds))
        (message "Selection saved to register '%c'" register))
    (copy-to-register register (line-beginning-position) (line-end-position))
    (message "Line saved to register '%c'" register)))

(defun eme-register-insert (register)
  "Insert text from REGISTER at point.
If eme-selection is active, replace selection with register content."
  (interactive
   (list (register-read-with-preview "Insert register: ")))
  (let ((val (get-register register)))
    (cond
     ((not val)
      (message "Register '%c' is empty" register))
     ((stringp val)
      (when (eme-selection-active-p)
        (let ((bounds (eme-selection-bounds)))
          (delete-region (car bounds) (cdr bounds))
          (eme-selection-clear)))
      (insert val))
     ((markerp val)
      (message "Register '%c' contains a position, not text" register))
     (t
      (message "Register '%c' cannot be inserted as text" register)))))

;;; Mode-line for Macro Recording

(defun eme--macro-mode-line ()
  "Return mode-line indicator for macro recording."
  (if defining-kbd-macro
      "[REC]"
    ""))

;; Add to mode-line if not already present
(unless (memq 'eme--macro-mode-line mode-line-misc-info)
  (push '(:eval (eme--macro-mode-line)) mode-line-misc-info))

(provide 'eme-macro-register)

;;; eme-macro-register.el ends here
