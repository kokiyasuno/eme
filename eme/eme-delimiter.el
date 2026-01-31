;;; eme-delimiter.el --- Delimiter operations for eme -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Koki Yasuno

;;; Commentary:

;; Unified delimiter operations with 2-stroke design:
;;
;; m - Select inner of nearest delimiter + remember context
;; M - Select bounds (including delimiters) of nearest delimiter
;;
;; After m selection (with context):
;; - Same delimiter key: delete delimiter (2 strokes)
;; - Different delimiter key: change delimiter (2 strokes)
;; - Action key (d/c/w/y): normal action on selection
;;
;; Wrap mode (selection exists, no delimiter context):
;; - m + delimiter key: wrap selection (3 strokes)
;;
;; m repeat: expand to outer delimiter

;;; Code:

(require 'eme-core)
(require 'eme-thing)

;; Forward declarations for byte-compiler
(defvar eme-delimiter-chars)
(defvar eme--delimiter-context)
(defvar eme--goal-column-valid)

;;; Delimiter Pairs

(defvar eme-delimiter-pairs
  '((?\( . ?\))
    (?\[ . ?\])
    (?\{ . ?\})
    (?\< . ?\>)
    (?\" . ?\")
    (?\' . ?\')
    (?\` . ?\`))
  "Alist of opening to closing delimiters.")

(defun eme--get-closing-delimiter (open)
  "Get the closing delimiter for OPEN."
  (or (cdr (assq open eme-delimiter-pairs))
      open))

(defun eme--get-opening-delimiter (close)
  "Get the opening delimiter for CLOSE."
  (or (car (seq-find (lambda (pair) (eq (cdr pair) close))
                     eme-delimiter-pairs))
      close))

(defun eme--normalize-delimiter (char)
  "Normalize CHAR to its opening delimiter form."
  (let ((open (eme--get-opening-delimiter char)))
    (if (memq open (mapcar #'car eme-delimiter-pairs))
        open
      char)))

;;; Selection Functions

(defun eme--delimiter-select-inner (char bounds)
  "Select inner content of delimiter CHAR with BOUNDS.
BOUNDS is (OPEN-POS . CLOSE-POS)."
  (let ((inner-start (1+ (car bounds)))
        (inner-end (cdr bounds)))
    (eme-selection-set inner-start inner-end)
    ;; Store context for subsequent operations
    (setq eme--delimiter-context (cons char bounds))))

(defun eme--delimiter-select-bounds (_char bounds)
  "Select bounds including delimiter with BOUNDS.
BOUNDS is (OPEN-POS . CLOSE-POS)."
  (let ((start (car bounds))
        (end (1+ (cdr bounds))))  ; Include closing delimiter
    (eme-selection-set start end)
    ;; Clear context - bounds selection doesn't set context
    (setq eme--delimiter-context nil)))

;;; Main Commands

(defun eme-delimiter-inner ()
  "Select inner of nearest delimiter and remember context.
If already in delimiter context, expand to outer delimiter.
If no outer delimiter found, keep current selection.
After selection, delimiter keys perform:
- Same delimiter: delete delimiter
- Different delimiter: change delimiter
- Action keys: normal action"
  (interactive)
  (setq eme--goal-column-valid nil)
  (let* (;; If expanding, find outer delimiter
         (current-bounds (when eme--delimiter-context
                          (cdr eme--delimiter-context)))
         (result (if current-bounds
                     ;; Get outer delimiter using thing API
                     (eme-thing-outer 'delimiter current-bounds)
                   ;; Get nearest delimiter
                   (eme-thing-bounds 'delimiter))))
    (if result
        (progn
          (eme-selection-clear)
          (let ((char (or (plist-get result :char) ?\())
                (inner (plist-get result :inner))
                (bounds (plist-get result :bounds)))
            (eme-selection-set (car inner) (cdr inner))
            (setq eme--delimiter-context (cons char bounds))
            ;; Install transient keymap for delimiter operations
            (eme--delimiter-install-transient-map)))
      ;; No delimiter found - keep current selection if expanding
      (unless eme--delimiter-context
        (message "No delimiter found")))))

(defun eme-delimiter-bounds ()
  "Select bounds of nearest delimiter (including delimiters).
If no delimiter found, keep current selection."
  (interactive)
  (setq eme--goal-column-valid nil)
  (let ((result (eme-thing-bounds 'delimiter)))
    (if result
        (progn
          (eme-selection-clear)
          (let ((bounds (plist-get result :bounds)))
            (eme-selection-set (car bounds) (cdr bounds))
            (setq eme--delimiter-context nil)))
      (message "No delimiter found"))))

;;; Transient Keymap for Delimiter Operations

(defvar eme--delimiter-transient-map nil
  "Transient keymap active after delimiter inner selection.")

(defun eme--delimiter-clear-context ()
  "Clear delimiter context."
  (setq eme--delimiter-context nil))

(defun eme--delimiter-handle-key ()
  "Handle key press after delimiter inner selection.
This is called for delimiter keys from `eme-delimiter-chars'."
  (interactive)
  (let* ((key (this-command-keys))
         (char (aref key 0))
         (normalized-char (eme--normalize-delimiter char)))
    (if eme--delimiter-context
        (let* ((ctx-char (car eme--delimiter-context))
               (ctx-bounds (cdr eme--delimiter-context)))
          (if (eq normalized-char ctx-char)
              ;; Same delimiter: delete it
              (eme--delimiter-delete ctx-bounds)
            ;; Different delimiter: change it
            (eme--delimiter-change ctx-bounds normalized-char))
          (eme--delimiter-clear-context)
          (eme-selection-clear))
      ;; No context (wrap mode)
      (eme--delimiter-wrap normalized-char)
      (eme-selection-clear))))

(defun eme--delimiter-delete (bounds)
  "Delete delimiters at BOUNDS, keeping inner content."
  (let ((open-pos (car bounds))
        (close-pos (cdr bounds)))
    (save-excursion
      ;; Delete closing first (positions stay valid)
      (goto-char close-pos)
      (delete-char 1)
      ;; Delete opening
      (goto-char open-pos)
      (delete-char 1))))

(defun eme--delimiter-change (bounds new-char)
  "Change delimiters at BOUNDS to NEW-CHAR."
  (let ((open-pos (car bounds))
        (close-pos (cdr bounds))
        (new-close (eme--get-closing-delimiter new-char)))
    (save-excursion
      ;; Replace closing first
      (goto-char close-pos)
      (delete-char 1)
      (insert (char-to-string new-close))
      ;; Replace opening
      (goto-char open-pos)
      (delete-char 1)
      (insert (char-to-string new-char)))))

(defun eme--delimiter-wrap (char)
  "Wrap current selection with CHAR."
  (let ((bounds (eme-selection-bounds)))
    (when bounds
      (let* ((close-char (eme--get-closing-delimiter char))
             (beg (car bounds))
             (end (cdr bounds))
             (text (buffer-substring beg end)))
        (delete-region beg end)
        (insert (char-to-string char))
        (insert text)
        (insert (char-to-string close-char))))))

(defvar eme--delimiter-transient-active nil
  "Non-nil when delimiter transient map is active.")

(defun eme--delimiter-install-transient-map ()
  "Install transient keymap for delimiter operations."
  ;; Don't reinstall if already active - just keep context
  (when eme--delimiter-transient-active
    (setq eme--delimiter-transient-active nil))
  ;; Build keymap dynamically based on eme-delimiter-chars
  (let ((map (make-sparse-keymap)))
    ;; Bind all delimiter characters
    (dolist (char eme-delimiter-chars)
      (define-key map (char-to-string char) #'eme--delimiter-handle-key))
    ;; Bind m for expansion to outer delimiter
    (define-key map "m" #'eme-delimiter-inner)
    (setq eme--delimiter-transient-map map)
    (setq eme--delimiter-transient-active t)
    ;; Use set-transient-map so it auto-deactivates
    (set-transient-map
     map
     ;; Keep active while pressing delimiter keys or m
     (lambda ()
       (or (eq this-command 'eme--delimiter-handle-key)
           (eq this-command 'eme-delimiter-inner)))
     ;; Cleanup when done
     (lambda ()
       (setq eme--delimiter-transient-active nil)
       (eme--delimiter-clear-context)))))

;;; Wrap Mode Entry

(defun eme-delimiter-wrap-or-inner ()
  "Smart m key: inner selection or wrap mode.
If selection exists without delimiter context: enter wrap mode.
Otherwise: select inner of nearest delimiter."
  (interactive)
  (if (and (eme-selection-active-p) (not eme--delimiter-context))
      ;; Wrap mode: wait for delimiter key
      (progn
        (message "Wrap with: ")
        (eme--delimiter-install-wrap-map))
    ;; Normal inner selection
    (eme-delimiter-inner)))

(defun eme--delimiter-install-wrap-map ()
  "Install transient keymap for wrap mode."
  (let ((map (make-sparse-keymap)))
    ;; Bind all delimiter characters for wrap
    (dolist (char eme-delimiter-chars)
      (define-key map (char-to-string char) #'eme--delimiter-handle-wrap-key))
    (set-transient-map map t)))

(defun eme--delimiter-handle-wrap-key ()
  "Handle delimiter key in wrap mode."
  (interactive)
  (let* ((key (this-command-keys))
         (char (aref key 0))
         (normalized-char (eme--normalize-delimiter char)))
    (eme--delimiter-wrap normalized-char)
    (eme-selection-clear)))

(provide 'eme-delimiter)

;;; eme-delimiter.el ends here
