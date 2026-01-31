;;; eme-selection.el --- Selection mode commands for eme -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Koki Yasuno

;;; Commentary:

;; Movement commands for Selection Mode that create selections.
;; All movement follows the unified selection model:
;; - Anchor inactive: create fresh selection from current position
;; - Anchor active: extend selection by merging with anchor bounds
;;
;; Granularity pattern (frequency-based key assignment):
;; - lowercase: most frequent (f/b = word, n/p = line)
;; - UPPERCASE: second (N/P = paragraph)
;; - M-key: third (M-n/M-p = defun, M-f/M-b = sexp)
;;
;; Line boundaries:
;; - lowercase: line (a/e)
;; - UPPERCASE: sentence (A/E)
;; - M-key: defun (M-a/M-e)

;;; Code:

(require 'eme-core)
(require 'eme-thing)

;;; Helper Functions

(defun eme--extend-selection-to (new-pos)
  "Extend eme-selection to include NEW-POS.
If selection exists, expands it.
Otherwise creates new selection from point to NEW-POS."
  (let* ((bounds (eme-selection-bounds))
         (start (if bounds (min (car bounds) new-pos (point)) (min (point) new-pos)))
         (end (if bounds (max (cdr bounds) new-pos (point)) (max (point) new-pos))))
    (eme-selection-set start end)))

;;; Word Movement (f/b keys)

(defun eme-forward-word ()
  "Move forward one word and select that word.
If anchor is active, merge with anchor-bounds.
Otherwise, select only the moved-to word."
  (interactive)
  (setq eme--goal-column-valid nil)
  (forward-word 1)
  (let ((result (eme-thing-bounds 'word)))
    (when result
      (let ((bounds (plist-get result :bounds)))
        (if eme--anchor-active
            (eme--merge-with-anchor (car bounds) (cdr bounds))
          (eme-selection-set (car bounds) (cdr bounds)))))))

(defun eme-backward-word ()
  "Move backward one word and select that word.
If anchor is active, merge with anchor-bounds.
Otherwise, select only the moved-to word."
  (interactive)
  (setq eme--goal-column-valid nil)
  (backward-word 1)
  (let ((result (eme-thing-bounds 'word)))
    (when result
      (let ((bounds (plist-get result :bounds)))
        (if eme--anchor-active
            (eme--merge-with-anchor (car bounds) (cdr bounds))
          (eme-selection-set (car bounds) (cdr bounds)))))))

;;; S-expression Movement

(defun eme-forward-sexp ()
  "Move forward one s-expression and select it.
If anchor is active, merge with anchor-bounds.
Otherwise, select only the moved-to sexp."
  (interactive)
  (setq eme--goal-column-valid nil)
  (condition-case nil
      (progn
        (forward-sexp 1)
        (let ((result (eme-thing-bounds 'sexp)))
          (when result
            (let ((bounds (plist-get result :bounds)))
              (if eme--anchor-active
                  (eme--merge-with-anchor (car bounds) (cdr bounds))
                (eme-selection-set (car bounds) (cdr bounds)))))))
    (scan-error nil)))

(defun eme-backward-sexp ()
  "Move backward one s-expression and select it.
If anchor is active, merge with anchor-bounds.
Otherwise, select only the moved-to sexp."
  (interactive)
  (setq eme--goal-column-valid nil)
  (condition-case nil
      (progn
        (backward-sexp 1)
        (let ((result (eme-thing-bounds 'sexp)))
          (when result
            (let ((bounds (plist-get result :bounds)))
              (if eme--anchor-active
                  (eme--merge-with-anchor (car bounds) (cdr bounds))
                (eme-selection-set (car bounds) (cdr bounds)))))))
    (scan-error nil)))

;;; Line Movement (n/p keys)
;; Line movement selects the entire destination line (beginning to end).
;; Column position is remembered across consecutive line movements.
;; Uses eme-selection (overlay) exclusively:
;;   - cursor at goal column
;;   - whole line visually selected
;; When anchor is set, merges anchor-bounds with new line bounds.

(defun eme-next-line ()
  "Move to the next line and select that line.
Column position is remembered across consecutive line movements.
If anchor is set, merge with anchor-bounds to extend selection."
  (interactive)
  ;; Remember column on first call of sequence
  (unless eme--goal-column-valid
    (setq eme--goal-column (current-column)))
  (setq eme--goal-column-valid t)
  (when (= (forward-line 1) 0)
    (let ((result (eme-thing-bounds 'line)))
      (when result
        (let ((bounds (plist-get result :bounds)))
          (if eme--anchor-active
              (eme--merge-with-anchor (car bounds) (cdr bounds))
            (eme-selection-set (car bounds) (cdr bounds))))))
    (move-to-column eme--goal-column)))

(defun eme-previous-line ()
  "Move to the previous line and select that line.
Column position is remembered across consecutive line movements.
If anchor is set, merge with anchor-bounds to extend selection."
  (interactive)
  ;; Remember column on first call of sequence
  (unless eme--goal-column-valid
    (setq eme--goal-column (current-column)))
  (setq eme--goal-column-valid t)
  (when (= (forward-line -1) 0)
    (let ((result (eme-thing-bounds 'line)))
      (when result
        (let ((bounds (plist-get result :bounds)))
          (if eme--anchor-active
              (eme--merge-with-anchor (car bounds) (cdr bounds))
            (eme-selection-set (car bounds) (cdr bounds))))))
    (move-to-column eme--goal-column)))

;;; Paragraph Movement

(defun eme-forward-paragraph ()
  "Move forward one paragraph and select."
  (interactive)
  (setq eme--goal-column-valid nil)
  (let ((start (point)))
    (condition-case nil
        (forward-paragraph 1)
      (end-of-buffer nil))
    (let ((end (point)))
      (if eme--anchor-active
          (eme--merge-with-anchor (min start end) (max start end))
        (eme-selection-set (min start end) (max start end))))))

(defun eme-backward-paragraph ()
  "Move backward one paragraph and select."
  (interactive)
  (setq eme--goal-column-valid nil)
  (let ((start (point)))
    (condition-case nil
        (backward-paragraph 1)
      (beginning-of-buffer nil))
    (let ((end (point)))
      (if eme--anchor-active
          (eme--merge-with-anchor (min start end) (max start end))
        (eme-selection-set (min start end) (max start end))))))

;;; Defun Movement

(defun eme-next-defun ()
  "Move to the next defun and select."
  (interactive)
  (setq eme--goal-column-valid nil)
  (let ((start (point)))
    (condition-case nil
        (progn
          (end-of-defun)
          (beginning-of-defun -1))
      (error nil))
    (let ((end (point)))
      (if eme--anchor-active
          (eme--merge-with-anchor (min start end) (max start end))
        (eme-selection-set (min start end) (max start end))))))

(defun eme-previous-defun ()
  "Move to the previous defun and select."
  (interactive)
  (setq eme--goal-column-valid nil)
  (let ((start (point)))
    (condition-case nil
        (beginning-of-defun 1)
      (error nil))
    (let ((end (point)))
      (if eme--anchor-active
          (eme--merge-with-anchor (min start end) (max start end))
        (eme-selection-set (min start end) (max start end))))))

;;; Line Boundary Movement

(defun eme-beginning-of-line ()
  "Move to the beginning of the line and select."
  (interactive)
  (setq eme--goal-column-valid nil)
  (let ((start (point)))
    (beginning-of-line)
    (let ((end (point)))
      (if eme--anchor-active
          (eme--merge-with-anchor (min start end) (max start end))
        (eme-selection-set (min start end) (max start end))))))

(defun eme-end-of-line ()
  "Move to the end of the line and select."
  (interactive)
  (setq eme--goal-column-valid nil)
  (let ((start (point)))
    (end-of-line)
    (let ((end (point)))
      (if eme--anchor-active
          (eme--merge-with-anchor (min start end) (max start end))
        (eme-selection-set (min start end) (max start end))))))

;;; Sentence Boundary Movement

(defun eme-beginning-of-sentence ()
  "Move to the beginning of the sentence and select."
  (interactive)
  (setq eme--goal-column-valid nil)
  (let ((start (point)))
    (condition-case nil
        (backward-sentence 1)
      (error nil))
    (let ((end (point)))
      (if eme--anchor-active
          (eme--merge-with-anchor (min start end) (max start end))
        (eme-selection-set (min start end) (max start end))))))

(defun eme-end-of-sentence ()
  "Move to the end of the sentence and select."
  (interactive)
  (setq eme--goal-column-valid nil)
  (let ((start (point)))
    (condition-case nil
        (forward-sentence 1)
      (error nil))
    (let ((end (point)))
      (if eme--anchor-active
          (eme--merge-with-anchor (min start end) (max start end))
        (eme-selection-set (min start end) (max start end))))))

;;; Defun Boundary Movement

(defun eme-beginning-of-defun ()
  "Move to the beginning of the defun and select."
  (interactive)
  (setq eme--goal-column-valid nil)
  (let ((start (point)))
    (condition-case nil
        (beginning-of-defun)
      (error nil))
    (let ((end (point)))
      (if eme--anchor-active
          (eme--merge-with-anchor (min start end) (max start end))
        (eme-selection-set (min start end) (max start end))))))

(defun eme-end-of-defun ()
  "Move to the end of the defun and select."
  (interactive)
  (setq eme--goal-column-valid nil)
  (let ((start (point)))
    (condition-case nil
        (end-of-defun)
      (error nil))
    (let ((end (point)))
      (if eme--anchor-active
          (eme--merge-with-anchor (min start end) (max start end))
        (eme-selection-set (min start end) (max start end))))))

;;; Anchor Helper

(defun eme--merge-with-anchor (new-start new-end)
  "Merge NEW-START to NEW-END with anchor-bounds and set eme-selection.
Returns the merged bounds as (START . END)."
  (if eme--anchor-bounds
      (let ((merged-start (min (car eme--anchor-bounds) new-start))
            (merged-end (max (cdr eme--anchor-bounds) new-end)))
        (eme-selection-set merged-start merged-end)
        (cons merged-start merged-end))
    ;; No anchor bounds, just set the new range
    (eme-selection-set new-start new-end)
    (cons new-start new-end)))

;;; Anchor Toggle

(defun eme-toggle-anchor ()
  "Toggle selection anchor.
If anchor is active, deactivate it and clear selections.
If anchor is not active, set anchor. If eme-selection exists,
remember its bounds; otherwise set anchor at current point."
  (interactive)
  (setq eme--goal-column-valid nil)
  (if eme--anchor-active
      ;; Deactivate anchor - clear everything
      (progn
        (setq eme--anchor-active nil)
        (setq eme--anchor-bounds nil)
        (eme-selection-clear))
    ;; Set anchor
    (setq eme--anchor-active t)
    (if (eme-selection-active-p)
        ;; Remember current selection bounds
        (setq eme--anchor-bounds (eme-selection-bounds))
      ;; No selection - set anchor at point (single position as bounds)
      (setq eme--anchor-bounds (cons (point) (point))))))

;;; Extending Selection Commands (for use with anchor)
;; These commands extend eme-selection by moving point and including the new position.

(defun eme-extend-forward-char ()
  "Extend selection forward one character."
  (interactive)
  (forward-char 1)
  (eme--extend-selection-to (point)))

(defun eme-extend-backward-char ()
  "Extend selection backward one character."
  (interactive)
  (backward-char 1)
  (eme--extend-selection-to (point)))

(defun eme-extend-forward-word ()
  "Extend selection forward one word."
  (interactive)
  (forward-word 1)
  (eme--extend-selection-to (point)))

(defun eme-extend-backward-word ()
  "Extend selection backward one word."
  (interactive)
  (backward-word 1)
  (eme--extend-selection-to (point)))

(defun eme-extend-forward-sexp ()
  "Extend selection forward one s-expression."
  (interactive)
  (condition-case nil
      (progn
        (forward-sexp 1)
        (eme--extend-selection-to (point)))
    (scan-error nil)))

(defun eme-extend-backward-sexp ()
  "Extend selection backward one s-expression."
  (interactive)
  (condition-case nil
      (progn
        (backward-sexp 1)
        (eme--extend-selection-to (point)))
    (scan-error nil)))

(defun eme-extend-next-line ()
  "Extend selection to the next line."
  (interactive)
  (condition-case nil
      (progn
        (line-move 1)
        (eme--extend-selection-to (point)))
    (error nil)))

(defun eme-extend-previous-line ()
  "Extend selection to the previous line."
  (interactive)
  (condition-case nil
      (progn
        (line-move -1)
        (eme--extend-selection-to (point)))
    (error nil)))

(defun eme-extend-forward-paragraph ()
  "Extend selection forward one paragraph."
  (interactive)
  (forward-paragraph 1)
  (eme--extend-selection-to (point)))

(defun eme-extend-backward-paragraph ()
  "Extend selection backward one paragraph."
  (interactive)
  (backward-paragraph 1)
  (eme--extend-selection-to (point)))

(defun eme-extend-beginning-of-line ()
  "Extend selection to the beginning of the line."
  (interactive)
  (beginning-of-line)
  (eme--extend-selection-to (point)))

(defun eme-extend-end-of-line ()
  "Extend selection to the end of the line."
  (interactive)
  (end-of-line)
  (eme--extend-selection-to (point)))

;;; Selection Manipulation

(defun eme-exchange-point-and-mark ()
  "Exchange point and mark in eme-selection.
Point moves to the other end of the selection."
  (interactive)
  (when (eme-selection-active-p)
    (let ((bounds (eme-selection-bounds)))
      (if (= (point) (car bounds))
          (goto-char (cdr bounds))
        (goto-char (car bounds))))))

(provide 'eme-selection)

;;; eme-selection.el ends here
