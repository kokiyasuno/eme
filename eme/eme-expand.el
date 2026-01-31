;;; eme-expand.el --- Expand region for eme -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Koki Yasuno

;;; Commentary:

;; Semantic expand/contract region functionality.
;; Uses eme-thing-all-bounds for semantic unit detection.
;;
;; v: expand selection to next semantic unit
;; V: contract selection to previous semantic unit
;;
;; Maintains history for contract operations.
;; History is cleared when cursor moves or other selection operations occur.

;;; Code:

(require 'eme-core)
(require 'eme-thing)

;;; State Variables

(defvar-local eme--expand-history nil
  "History of expand bounds for contract operations.
Each element is (START . END).")

(defvar-local eme--expand-last-point nil
  "Point position when last expand occurred.
Used to detect when history should be cleared.")

;;; History Management

(defun eme--expand-clear-history ()
  "Clear expand history."
  (setq eme--expand-history nil)
  (setq eme--expand-last-point nil))

(defun eme--expand-maybe-clear-history ()
  "Clear history if point has moved since last expand."
  (when (and eme--expand-last-point
             (not (eq (point) eme--expand-last-point)))
    (eme--expand-clear-history)))

(defun eme--expand-push-history (start end)
  "Push bounds START END to expand history."
  (push (cons start end) eme--expand-history))

;;; Semantic Unit Detection (using eme-thing API)

(defun eme--expand-bounds-at-point ()
  "Get list of possible expansion bounds at point.
Returns list of (START . END) in order from smallest to largest.
Delegates to `eme-thing-all-bounds'."
  (let ((results nil))
    (dolist (thing-result (eme-thing-all-bounds))
      (push (plist-get thing-result :bounds) results))
    (nreverse results)))

(defun eme--expand-find-next-bounds (current-start current-end)
  "Find the next larger bounds that contain CURRENT-START to CURRENT-END.
Returns (START . END) or nil if no larger bounds found."
  (let ((all-bounds (eme--expand-bounds-at-point))
        (result nil))
    (dolist (bounds all-bounds)
      (when (and (<= (car bounds) current-start)
                 (>= (cdr bounds) current-end)
                 (or (< (car bounds) current-start)
                     (> (cdr bounds) current-end)))
        (unless result
          (setq result bounds))))
    result))

;;; Expand/Contract Functions

(defun eme-expand-region (&optional n)
  "Expand selection to the next semantic unit.
With numeric prefix N, expand N times.
Expansion order: word → symbol → string → list → defun.
If eme-selection from other commands (e.g., n/p line selection) is active
and we're not in the middle of expand-region, clear it first."
  (interactive "p")
  ;; Clear foreign selections (from n/p etc.) but preserve our own
  ;; We're continuing expansion if last-point matches current point
  (when (and (eme-selection-active-p)
             (not (eq eme--expand-last-point (point))))
    (eme-selection-clear)
    (eme--expand-clear-history))
  (eme--expand-maybe-clear-history)
  (let ((count (or n 1)))
    (dotimes (_ count)
      (let* ((bounds (eme-selection-bounds))
             (current-start (if bounds (car bounds) (point)))
             (current-end (if bounds (cdr bounds) (point)))
             (next-bounds (eme--expand-find-next-bounds current-start current-end)))
        (if next-bounds
            (progn
              ;; Push current bounds to history before expanding
              (when bounds
                (eme--expand-push-history current-start current-end))
              ;; Select new bounds using eme-selection
              (eme-selection-set (car next-bounds) (cdr next-bounds))
              (setq eme--expand-last-point (point)))
          ;; No larger bounds - stop expanding silently
          (setq count 0))))))

(defun eme-contract-region (&optional n)
  "Contract selection to the previous semantic unit.
With numeric prefix N, contract N times.
Uses expand history to restore previous selection."
  (interactive "p")
  (let ((count (or n 1)))
    (dotimes (_ count)
      (if eme--expand-history
          (let ((prev-bounds (pop eme--expand-history)))
            (eme-selection-set (car prev-bounds) (cdr prev-bounds))
            (setq eme--expand-last-point (point)))
        ;; No history - clear selection if at minimum
        (when (eme-selection-active-p)
          (eme-selection-clear)
          (setq count 0))))))

;;; Hook to clear history on certain events

(defun eme--expand-pre-command-hook ()
  "Clear expand history if command is not expand/contract."
  (unless (memq this-command '(eme-expand-region eme-contract-region))
    (eme--expand-clear-history)))

;; Install the hook when the mode is active
(add-hook 'eme-selection-mode-hook
          (lambda ()
            (if eme-selection-mode
                (add-hook 'pre-command-hook #'eme--expand-pre-command-hook nil t)
              (remove-hook 'pre-command-hook #'eme--expand-pre-command-hook t))))

(provide 'eme-expand)

;;; eme-expand.el ends here
