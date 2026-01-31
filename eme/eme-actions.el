;;; eme-actions.el --- Action commands for eme -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Koki Yasuno

;;; Commentary:

;; Action commands that operate on selections:
;; - d: delete (save to kill-ring)
;; - D: delete (no save)
;; - w: copy (yank in Emacs terminology is paste, so we use w for copy)
;; - y: yank (paste)
;; - c: change (delete + insert mode)
;; - x: replace with character
;; - ;: toggle comment
;; - =: format/indent
;; - >/<: increase/decrease indent
;; - j: join lines
;; - o/O: open line below/above
;; - +: duplicate
;; - `: toggle case
;; - u/U: undo/redo

;;; Code:

(require 'eme-core)
(require 'eme-thing)

;;; Selection Helper

(defun eme--get-selection-bounds ()
  "Get selection bounds from eme-selection.
Returns (BEG . END) or nil if no selection."
  (eme-selection-bounds))

(defun eme--clear-selection ()
  "Clear eme-selection and reset anchor."
  (eme-selection-clear)
  (setq eme--anchor-active nil)
  (setq eme--anchor-bounds nil))

;;; Delete Actions

(defun eme-delete ()
  "Delete selection and save to kill-ring.
If no selection is active, delete character at point."
  (interactive)
  (let ((bounds (eme--get-selection-bounds)))
    (if bounds
        (progn
          (kill-region (car bounds) (cdr bounds))
          (eme--clear-selection))
      (kill-region (point) (1+ (point))))))

(defun eme-delete-no-save ()
  "Delete selection without saving to kill-ring.
If no selection is active, delete character at point."
  (interactive)
  (let ((bounds (eme--get-selection-bounds)))
    (if bounds
        (progn
          (delete-region (car bounds) (cdr bounds))
          (eme--clear-selection))
      (delete-char 1))))

;;; Copy Action

(defun eme-copy ()
  "Copy selection to kill-ring.
If no selection is active, copy current line.
Selection remains active after copying."
  (interactive)
  (let ((bounds (eme--get-selection-bounds)))
    (if bounds
        (let ((beg (car bounds))
              (end (cdr bounds))
              (was-eme-selection (eme-selection-active-p)))
          ;; Copy to kill-ring
          (kill-ring-save beg end)
          ;; Ensure selection stays active
          (when was-eme-selection
            (eme-selection-set beg end)))
      (let ((line-result (eme-thing-bounds 'line)))
        (when line-result
          (let ((line-bounds (plist-get line-result :bounds)))
            (kill-ring-save (car line-bounds) (cdr line-bounds))
            (message "Copied line")))))))

;;; Yank Action

(defun eme-yank ()
  "Yank (paste) from kill-ring.
If selection is active, replace selection with yanked text."
  (interactive)
  (let ((bounds (eme--get-selection-bounds)))
    (when bounds
      (delete-region (car bounds) (cdr bounds))
      (eme--clear-selection))
    (yank)))

;;; Change Action

(defun eme-change ()
  "Delete selection and enter Insert Mode.
If no selection is active, just enter Insert Mode."
  (interactive)
  (let ((bounds (eme--get-selection-bounds)))
    (when bounds
      (kill-region (car bounds) (cdr bounds))
      (eme--clear-selection)))
  (eme-enter-insert-mode))

;;; Replace Character Action

(defun eme-replace-char (char)
  "Replace selection with CHAR repeated.
If no selection is active, replace character at point."
  (interactive "cReplace with: ")
  (let ((bounds (eme--get-selection-bounds)))
    (if bounds
        (let ((len (- (cdr bounds) (car bounds))))
          (delete-region (car bounds) (cdr bounds))
          (insert (make-string len char))
          (eme--clear-selection))
      (delete-char 1)
      (insert char)
      (backward-char 1))))

;;; Comment Toggle Action

(defun eme-comment-toggle ()
  "Toggle comment on selection or current line."
  (interactive)
  (let ((bounds (eme--get-selection-bounds)))
    (if bounds
        (progn
          (comment-or-uncomment-region (car bounds) (cdr bounds))
          (eme--clear-selection))
      (let ((line-result (eme-thing-bounds 'line)))
        (when line-result
          (let ((line-bounds (plist-get line-result :bounds)))
            (comment-or-uncomment-region (car line-bounds) (cdr line-bounds))))))))

;;; Format Action

(defun eme-format ()
  "Format/indent selection or current line."
  (interactive)
  (let ((bounds (eme--get-selection-bounds)))
    (if bounds
        (progn
          (indent-region (car bounds) (cdr bounds))
          (eme--clear-selection))
      (indent-according-to-mode))))

;;; Indent Actions

(defun eme-indent-increase ()
  "Increase indent of selection or current line."
  (interactive)
  (let* ((bounds (eme--get-selection-bounds))
         (line-result (unless bounds (eme-thing-bounds 'line)))
         (line-bounds (when line-result (plist-get line-result :bounds)))
         (beg (if bounds (car bounds) (car line-bounds)))
         (end (if bounds (cdr bounds) (cdr line-bounds))))
    (when (and beg end)
      (indent-rigidly beg end tab-width)
      (when bounds (eme--clear-selection)))))

(defun eme-indent-decrease ()
  "Decrease indent of selection or current line."
  (interactive)
  (let* ((bounds (eme--get-selection-bounds))
         (line-result (unless bounds (eme-thing-bounds 'line)))
         (line-bounds (when line-result (plist-get line-result :bounds)))
         (beg (if bounds (car bounds) (car line-bounds)))
         (end (if bounds (cdr bounds) (cdr line-bounds))))
    (when (and beg end)
      (indent-rigidly beg end (- tab-width))
      (when bounds (eme--clear-selection)))))

;;; Join Lines Action

(defun eme-join-lines ()
  "Join selected lines or join current line with next."
  (interactive)
  (let ((bounds (eme--get-selection-bounds)))
    (if bounds
        (let ((beg (car bounds))
              (end (cdr bounds)))
          (eme--clear-selection)
          (goto-char beg)
          (while (< (point) (min end (point-max)))
            (end-of-line)
            (when (< (point) (min end (point-max)))
              (delete-char 1)
              (just-one-space)
              (setq end (- end 1)))))
      (end-of-line)
      (when (< (point) (point-max))
        (delete-char 1)
        (just-one-space)))))

;;; Open Line Actions

(defun eme-open-line-below ()
  "Open a new line below current line and enter Insert Mode."
  (interactive)
  (end-of-line)
  (newline-and-indent)
  (eme-enter-insert-mode))

(defun eme-open-line-above ()
  "Open a new line above current line and enter Insert Mode."
  (interactive)
  (beginning-of-line)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode)
  (eme-enter-insert-mode))

;;; Duplicate Action

(defun eme-duplicate ()
  "Duplicate selection or current line."
  (interactive)
  (let ((bounds (eme--get-selection-bounds)))
    (if bounds
        (let ((text (buffer-substring (car bounds) (cdr bounds))))
          (eme--clear-selection)
          (goto-char (cdr bounds))
          (insert text))
      (let ((line-result (eme-thing-bounds 'line)))
        (when line-result
          (let* ((line-bounds (plist-get line-result :bounds))
                 (line (buffer-substring (car line-bounds) (cdr line-bounds))))
            (end-of-line)
            (newline)
            (insert line)))))))

;;; Toggle Case Action

(defun eme-toggle-case ()
  "Toggle case of selection or character at point.
Each character's case is toggled individually."
  (interactive)
  (let ((bounds (eme--get-selection-bounds)))
    (if bounds
        (let ((beg (car bounds))
              (end (cdr bounds)))
          (eme--clear-selection)
          (save-excursion
            (goto-char beg)
            (while (< (point) end)
              (let ((char (char-after)))
                (delete-char 1)
                (insert (if (eq char (upcase char))
                            (downcase char)
                          (upcase char)))))))
      (let ((char (char-after)))
        (delete-char 1)
        (insert (if (eq char (upcase char))
                    (downcase char)
                  (upcase char)))
        (backward-char 1)))))

;;; Undo/Redo Actions

(defun eme-undo ()
  "Undo the last edit."
  (interactive)
  (eme-selection-clear)
  (setq eme--goal-column-valid nil)
  (setq eme--anchor-active nil)
  (setq eme--anchor-bounds nil)
  (undo))

(defun eme-redo ()
  "Redo the last undo.
Uses `undo-redo' if available (Emacs 28+), otherwise uses undo."
  (interactive)
  (eme-selection-clear)
  (setq eme--goal-column-valid nil)
  (setq eme--anchor-active nil)
  (setq eme--anchor-bounds nil)
  (if (fboundp 'undo-redo)
      (undo-redo)
    ;; Fallback for older Emacs
    (let ((undo-in-progress t))
      (undo))))

;;; Sort Lines Action

(defun eme-sort-lines ()
  "Sort selected lines alphabetically."
  (interactive)
  (let ((bounds (eme--get-selection-bounds)))
    (if bounds
        (progn
          (sort-lines nil (car bounds) (cdr bounds))
          (eme--clear-selection))
      (message "No selection for sorting"))))

;;; Reverse Lines Action

(defun eme-reverse-lines ()
  "Reverse the order of selected lines."
  (interactive)
  (let ((bounds (eme--get-selection-bounds)))
    (if bounds
        (progn
          (reverse-region (car bounds) (cdr bounds))
          (eme--clear-selection))
      (message "No selection for reversing"))))

(provide 'eme-actions)

;;; eme-actions.el ends here
