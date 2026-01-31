;;; eme-thing.el --- Central thing provider for eme -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Koki Yasuno

;;; Commentary:

;; Central thing provider system for eme.
;; Provides unified API for detecting bounds of various text objects:
;; - word, symbol, sexp, sexp-chain
;; - line, sentence, paragraph, defun, buffer
;; - string, list, delimiter
;;
;; All thing detection uses tree-sitter when available, with syntax-based fallback.
;;
;; Public API:
;; - `eme-thing-bounds' - Get bounds for a specific thing
;; - `eme-thing-all-bounds' - Get all possible bounds at point
;; - `eme-thing-outer' - Get outer bounds of same thing type
;; - `eme-thing-providers' - Registry for thing providers (customizable)
;;
;; Legacy API (for backward compatibility):
;; - `eme-thing-inner' - Select inner of nearest delimiter (t i)
;; - `eme-thing-bounds' (interactive) - Select bounds of nearest delimiter (t b)

;;; Code:

(require 'eme-core)

;; Forward declarations for tree-sitter functions (available at runtime in Emacs 29+)
(declare-function treesit-node-at "treesit")
(declare-function treesit-parser-list "treesit")
(declare-function treesit-parser-root-node "treesit")
(declare-function treesit-node-eq "treesit")
(declare-function treesit-node-parent "treesit")
(declare-function treesit-node-start "treesit")
(declare-function treesit-node-end "treesit")
(declare-function treesit-node-type "treesit")
(declare-function treesit-node-text "treesit")
(declare-function treesit-available-p "treesit")

;;; Customization

(defcustom eme-thing-cursor-type 'hollow
  "Cursor type when in thing selection transient state."
  :type '(choice (const :tag "Hollow" hollow)
                 (const :tag "Box" box)
                 (const :tag "Bar" bar))
  :group 'eme)

;;; Provider Registry

(defvar eme-thing-providers
  '((word       . eme-thing--word)
    (symbol     . eme-thing--symbol)
    (sexp       . eme-thing--sexp)
    (sexp-chain . eme-thing--sexp-chain)
    (line       . eme-thing--line)
    (sentence   . eme-thing--sentence)
    (paragraph  . eme-thing--paragraph)
    (defun      . eme-thing--defun)
    (buffer     . eme-thing--buffer)
    (string     . eme-thing--string)
    (list       . eme-thing--list)
    (delimiter  . eme-thing--delimiter))
  "Alist mapping thing types to provider functions.
Each provider function takes an optional ARG and returns a plist:
  (:type TYPE :bounds (START . END) :inner (START . END) :char CHAR)
where :inner and :char are optional.")

;;; Tree-sitter Availability (with buffer-local cache)

(defvar-local eme-thing--treesit-available nil
  "Cached tree-sitter availability for current buffer.
nil = not checked, t = available, \\='unavailable = not available.")

(defun eme-thing--treesit-available-p ()
  "Return non-nil if tree-sitter is available for current buffer.
Result is cached per buffer for performance."
  (when (eq eme-thing--treesit-available nil)
    (setq eme-thing--treesit-available
          (if (and (fboundp 'treesit-available-p)
                   (treesit-available-p)
                   (fboundp 'treesit-parser-list)
                   (treesit-parser-list))
              t
            'unavailable)))
  (eq eme-thing--treesit-available t))

;;; Tree-sitter Utilities (migrated from eme-treesit.el)

(defun eme-thing--treesit-node-at-point ()
  "Get the smallest tree-sitter node at point."
  (when (eme-thing--treesit-available-p)
    (treesit-node-at (point))))

(defun eme-thing--treesit-parent-nodes ()
  "Get list of parent nodes from current position to root.
Returns list of nodes from smallest to largest."
  (when (eme-thing--treesit-available-p)
    (let ((node (treesit-node-at (point)))
          (root (treesit-parser-root-node (car (treesit-parser-list))))
          (nodes nil))
      (while (and node (not (treesit-node-eq node root)))
        (push node nodes)
        (setq node (treesit-node-parent node)))
      (nreverse nodes))))

(defun eme-thing--treesit-expansion-bounds ()
  "Get list of expansion bounds from tree-sitter AST.
Returns list of (START . END) sorted by size (smallest first)."
  (when (eme-thing--treesit-available-p)
    (let ((nodes (eme-thing--treesit-parent-nodes))
          (bounds-list nil)
          (seen (make-hash-table :test 'equal)))
      (dolist (node nodes)
        (let* ((start (treesit-node-start node))
               (end (treesit-node-end node))
               (key (format "%d-%d" start end)))
          ;; Skip duplicates and single-char nodes
          (when (and (> (- end start) 1)
                     (not (gethash key seen)))
            (puthash key t seen)
            (push (cons start end) bounds-list))))
      ;; Sort by size (smallest first)
      (sort bounds-list
            (lambda (a b)
              (< (- (cdr a) (car a))
                 (- (cdr b) (car b))))))))

(defun eme-thing--treesit-find-enclosing-delimiters ()
  "Find all enclosing delimiters using tree-sitter.
Returns list of (CHAR . (OPEN-POS . CLOSE-POS)) from innermost to outermost."
  (when (eme-thing--treesit-available-p)
    (let ((nodes (eme-thing--treesit-parent-nodes))
          (results nil)
          (seen (make-hash-table :test 'equal)))
      (dolist (node nodes)
        (let ((type (treesit-node-type node))
              (start (treesit-node-start node))
              (end (treesit-node-end node)))
          ;; String nodes
          (when (and (or (string= type "string")
                         (string= type "template_string")
                         (string= type "string_literal"))
                     (not (gethash start seen)))
            (let ((open-char (char-after start)))
              (when (memq open-char '(?\" ?\' ?\`))
                (puthash start t seen)
                (push (cons open-char (cons start (1- end)))
                      results))))
          ;; Bracket nodes
          (when (and (or (string-match-p "parenthesized" type)
                         (string-match-p "arguments" type)
                         (string-match-p "parameters" type)
                         (string-match-p "statement_block" type)
                         (string-match-p "class_body" type)
                         (string-match-p "block" type)
                         (string-match-p "array" type)
                         (string-match-p "object" type)
                         (string-match-p "body" type)
                         (string-match-p "list" type))
                     (not (gethash start seen)))
            (let ((open-char (char-after start)))
              (when (memq open-char '(?\( ?\[ ?\{ ?\<))
                (puthash start t seen)
                (push (cons open-char (cons start (1- end)))
                      results))))))
      (nreverse results))))

;;; Standard Thing Providers

(defun eme-thing--word (&optional _arg)
  "Provider for word thing."
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (when bounds
      (list :type 'word :bounds bounds :inner nil))))

(defun eme-thing--symbol (&optional _arg)
  "Provider for symbol thing."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (list :type 'symbol :bounds bounds :inner nil))))

(defun eme-thing--sexp (&optional _arg)
  "Provider for sexp thing."
  (save-excursion
    (condition-case nil
        (let (start end)
          ;; Try to find sexp at point
          (when (looking-at-p "\\s(\\|\\s\"")
            (setq start (point))
            (forward-sexp 1)
            (setq end (point))
            (list :type 'sexp :bounds (cons start end) :inner nil)))
      (scan-error nil))))

(defun eme-thing--sexp-chain (&optional _arg)
  "Provider for sexp-chain thing.
A sexp-chain is a symbol with function calls, array access, method chains.
E.g., foo.bar()[0].baz"
  (let ((symbol-bounds (bounds-of-thing-at-point 'symbol)))
    (when symbol-bounds
      (save-excursion
        (let ((chain-start (car symbol-bounds))
              (chain-end (cdr symbol-bounds)))
          ;; Look backward for dot-chains
          (goto-char (car symbol-bounds))
          (while (and (> (point) (point-min))
                      (eq (char-before) ?\.))
            (backward-char 1)
            (let ((prev-symbol (bounds-of-thing-at-point 'symbol)))
              (if prev-symbol
                  (progn
                    (setq chain-start (car prev-symbol))
                    (goto-char (car prev-symbol)))
                (forward-char 1))))
          ;; Look forward for chains
          (goto-char (cdr symbol-bounds))
          (let ((continue t))
            (while continue
              (let ((next-char (char-after)))
                (cond
                 ((memq next-char '(?\( ?\[))
                  (condition-case nil
                      (progn
                        (forward-sexp 1)
                        (setq chain-end (point)))
                    (scan-error (setq continue nil))))
                 ((eq next-char ?\.)
                  (forward-char 1)
                  (let ((next-symbol (bounds-of-thing-at-point 'symbol)))
                    (if next-symbol
                        (progn
                          (goto-char (cdr next-symbol))
                          (setq chain-end (cdr next-symbol)))
                      (setq continue nil))))
                 (t (setq continue nil))))))
          ;; Only return if chain extends beyond original symbol
          (when (or (< chain-start (car symbol-bounds))
                    (> chain-end (cdr symbol-bounds)))
            (list :type 'sexp-chain :bounds (cons chain-start chain-end) :inner nil)))))))

(defun eme-thing--line (&optional _arg)
  "Provider for line thing."
  (list :type 'line
        :bounds (cons (line-beginning-position) (line-end-position))
        :inner nil))

(defun eme-thing--sentence (&optional _arg)
  "Provider for sentence thing."
  (save-excursion
    (condition-case nil
        (let ((start (progn (backward-sentence 1) (point)))
              (end (progn (forward-sentence 1) (point))))
          (list :type 'sentence :bounds (cons start end) :inner nil))
      (error nil))))

(defun eme-thing--paragraph (&optional _arg)
  "Provider for paragraph thing."
  (save-excursion
    (condition-case nil
        (let ((start (progn (backward-paragraph 1) (point)))
              (end (progn (forward-paragraph 1) (point))))
          (list :type 'paragraph :bounds (cons start end) :inner nil))
      (error nil))))

(defun eme-thing--defun (&optional _arg)
  "Provider for defun thing."
  (save-excursion
    (condition-case nil
        (let ((pos (point))
              (start (progn (beginning-of-defun) (point)))
              (end (progn (end-of-defun) (point))))
          (when (and (< start pos) (> end pos))
            (list :type 'defun :bounds (cons start end) :inner nil)))
      (error nil))))

(defun eme-thing--buffer (&optional _arg)
  "Provider for buffer thing."
  (list :type 'buffer
        :bounds (cons (point-min) (point-max))
        :inner nil))

(defun eme-thing--string (&optional _arg)
  "Provider for string thing."
  ;; Try tree-sitter first
  (or (when (eme-thing--treesit-available-p)
        (let ((delimiters (eme-thing--treesit-find-enclosing-delimiters))
              (result nil))
          (catch 'found
            (dolist (d delimiters)
              (when (memq (car d) '(?\" ?\' ?\`))
                (let* ((open-pos (cadr d))
                       (close-pos (1+ (cddr d))))
                  (setq result (list :type 'string
                                     :bounds (cons open-pos close-pos)
                                     :inner (cons (1+ open-pos) (1- close-pos))
                                     :char (car d)))
                  (throw 'found t)))))
          result))
      ;; Fallback to syntax-ppss
      ;; First check if we're at an opening quote
      (let ((char-at-point (char-after))
            (syntax (syntax-class (syntax-after (point)))))
        (if (and (memq char-at-point '(?\" ?\' ?\`))
                 (eq syntax 7))  ; string quote syntax class
            ;; At opening quote - use this as the string
            (save-excursion
              (let ((start (point)))
                (condition-case nil
                    (progn
                      (forward-sexp 1)
                      (list :type 'string
                            :bounds (cons start (point))
                            :inner (cons (1+ start) (1- (point)))
                            :char char-at-point))
                  (scan-error nil))))
          ;; Not at opening quote - use syntax-ppss
          (let ((ppss (syntax-ppss)))
            (when (nth 3 ppss)
              (let ((start (nth 8 ppss)))
                (save-excursion
                  (goto-char start)
                  (condition-case nil
                      (progn
                        (forward-sexp 1)
                        (list :type 'string
                              :bounds (cons start (point))
                              :inner (cons (1+ start) (1- (point)))
                              :char (char-after start)))
                    (scan-error nil))))))))))

(defun eme-thing--list (&optional _arg)
  "Provider for list/paren thing."
  ;; Try tree-sitter first
  (or (when (eme-thing--treesit-available-p)
        (let ((delimiters (eme-thing--treesit-find-enclosing-delimiters))
              (result nil))
          (catch 'found
            (dolist (d delimiters)
              (when (memq (car d) '(?\( ?\[ ?\{ ?\<))
                (let* ((open-pos (cadr d))
                       (close-pos (1+ (cddr d))))
                  (setq result (list :type 'list
                                     :bounds (cons open-pos close-pos)
                                     :inner (cons (1+ open-pos) (1- close-pos))
                                     :char (car d)))
                  (throw 'found t)))))
          result))
      ;; Fallback to syntax-ppss
      ;; First check if we're at an opening bracket
      (let ((char-at-point (char-after)))
        (if (memq char-at-point '(?\( ?\[ ?\{ ?\<))
            ;; At opening bracket - use this as the list
            (save-excursion
              (let ((start (point)))
                (condition-case nil
                    (progn
                      (forward-sexp 1)
                      (list :type 'list
                            :bounds (cons start (point))
                            :inner (cons (1+ start) (1- (point)))
                            :char char-at-point))
                  (scan-error nil))))
          ;; Not at opening bracket - use syntax-ppss
          (let ((ppss (syntax-ppss)))
            (when (nth 1 ppss)
              (let ((start (nth 1 ppss)))
                (save-excursion
                  (goto-char start)
                  (condition-case nil
                      (progn
                        (forward-sexp 1)
                        (list :type 'list
                              :bounds (cons start (point))
                              :inner (cons (1+ start) (1- (point)))
                              :char (char-after start)))
                    (scan-error nil))))))))))

(defun eme-thing--delimiter (&optional char)
  "Provider for delimiter thing.
If CHAR is provided, find that specific delimiter.
Otherwise, auto-detect the nearest enclosing delimiter."
  (if char
      ;; Find specific delimiter
      (eme-thing--delimiter-specific char)
    ;; Auto-detect nearest
    (or (eme-thing--string)
        (eme-thing--list))))

(defun eme-thing--delimiter-specific (char)
  "Find enclosing delimiter matching CHAR."
  (let ((close-char (eme-thing--get-matching-char char))
        (same-char-p (eq char (eme-thing--get-matching-char char))))
    (if same-char-p
        ;; Quote-like delimiter
        (eme-thing--find-enclosing-quote char)
      ;; Bracket-like delimiter
      (eme-thing--find-enclosing-bracket char close-char))))

(defun eme-thing--get-matching-char (char)
  "Get the matching delimiter for CHAR."
  (pcase char
    (?\( ?\))
    (?\) ?\()
    (?\[ ?\])
    (?\] ?\[)
    (?\{ ?\})
    (?\} ?\{)
    (?\< ?\>)
    (?\> ?\<)
    (_ char)))

(defun eme-thing--find-enclosing-quote (char)
  "Find enclosing quotes matching CHAR."
  (save-excursion
    (let ((start-pos (point))
          (open-pos nil)
          (close-pos nil))
      ;; Find opening quote backward
      (while (and (not open-pos) (not (bobp)))
        (backward-char 1)
        (when (eq (char-after) char)
          (setq open-pos (point))))
      ;; Find closing quote forward
      (when open-pos
        (goto-char start-pos)
        (while (and (not close-pos) (not (eobp)))
          (when (and (eq (char-after) char)
                     (not (eq (point) open-pos)))
            (setq close-pos (1+ (point))))
          (forward-char 1))
        (when close-pos
          (list :type 'delimiter
                :bounds (cons open-pos close-pos)
                :inner (cons (1+ open-pos) (1- close-pos))
                :char char))))))

(defun eme-thing--find-enclosing-bracket (open-char close-char)
  "Find enclosing brackets OPEN-CHAR/CLOSE-CHAR."
  (save-excursion
    (let ((open-pos nil)
          (depth 0))
      ;; Find opening bracket backward
      (while (and (not open-pos) (not (bobp)))
        (backward-char 1)
        (cond
         ((eq (char-after) close-char)
          (setq depth (1+ depth)))
         ((eq (char-after) open-char)
          (if (> depth 0)
              (setq depth (1- depth))
            (setq open-pos (point))))))
      ;; Find matching close
      (when open-pos
        (goto-char open-pos)
        (condition-case nil
            (progn
              (forward-sexp 1)
              (list :type 'delimiter
                    :bounds (cons open-pos (point))
                    :inner (cons (1+ open-pos) (1- (point)))
                    :char open-char))
          (scan-error nil))))))

;;; Public API

(defun eme-thing-bounds (thing &optional arg)
  "Get bounds for THING at point.
THING is a symbol like \\='word, \\='symbol, \\='delimiter, etc.
ARG is passed to the provider (e.g., specific delimiter char).
Returns a plist (:type TYPE :bounds (START . END) :inner (START . END))
or nil if thing not found."
  (let ((provider (cdr (assq thing eme-thing-providers))))
    (when provider
      (funcall provider arg))))

(defun eme-thing-all-bounds ()
  "Get all possible thing bounds at point, sorted by size.
Returns list of plists, smallest bounds first."
  (let ((results nil)
        (seen (make-hash-table :test 'equal)))
    ;; Collect from all providers
    (dolist (entry eme-thing-providers)
      (let ((result (funcall (cdr entry))))
        (when result
          (let* ((bounds (plist-get result :bounds))
                 (key (format "%d-%d" (car bounds) (cdr bounds))))
            (unless (gethash key seen)
              (puthash key t seen)
              (push result results))))))
    ;; Add tree-sitter bounds if available
    (when (eme-thing--treesit-available-p)
      (dolist (bounds (eme-thing--treesit-expansion-bounds))
        (let ((key (format "%d-%d" (car bounds) (cdr bounds))))
          (unless (gethash key seen)
            (puthash key t seen)
            (push (list :type 'treesit-node :bounds bounds :inner nil) results)))))
    ;; Sort by size (smallest first)
    (sort results
          (lambda (a b)
            (let ((a-bounds (plist-get a :bounds))
                  (b-bounds (plist-get b :bounds)))
              (< (- (cdr a-bounds) (car a-bounds))
                 (- (cdr b-bounds) (car b-bounds))))))))

(defun eme-thing-outer (thing current-bounds &optional arg)
  "Get outer bounds of THING containing CURRENT-BOUNDS.
Returns plist or nil if no outer thing found."
  (save-excursion
    ;; Move just before current bounds
    (goto-char (1- (car current-bounds)))
    (let ((result (eme-thing-bounds thing arg)))
      ;; Ensure it's actually larger
      (when (and result
                 (let ((new-bounds (plist-get result :bounds)))
                   (and (< (car new-bounds) (car current-bounds))
                        (> (cdr new-bounds) (cdr current-bounds)))))
        result))))

;;; State Variables (for legacy API)

(defvar-local eme--thing-last-type nil
  "Last thing selection type (inner or bounds).")

(defvar-local eme--thing-last-bounds nil
  "Last thing selection bounds as (START . END).")

;;; Legacy API (Backward Compatibility)

(defun eme--select-bounds (bounds)
  "Select BOUNDS using eme-selection overlay.
BOUNDS is (START . END)."
  (when bounds
    (goto-char (car bounds))
    (eme-selection-set (car bounds) (cdr bounds))))

(defun eme-thing-inner ()
  "Select inner of nearest enclosing delimiter.
The delimiter itself is not included in the selection."
  (interactive)
  (let ((result (eme-thing-bounds 'delimiter)))
    (if result
        (let ((inner (plist-get result :inner))
              (bounds (plist-get result :bounds)))
          (when inner
            (setq eme--thing-last-type 'inner)
            (setq eme--thing-last-bounds bounds)
            (eme--select-bounds inner)
            (eme--thing-setup-transient-map)))
      (message "No enclosing delimiter found"))))

(defun eme-thing-bounds-select ()
  "Select bounds of nearest enclosing delimiter.
The delimiter itself is included in the selection."
  (interactive)
  (let ((result (eme-thing-bounds 'delimiter)))
    (if result
        (let ((bounds (plist-get result :bounds)))
          (setq eme--thing-last-type 'bounds)
          (setq eme--thing-last-bounds bounds)
          (eme--select-bounds bounds)
          (eme--thing-setup-transient-map))
      (message "No enclosing delimiter found"))))

(defun eme-thing-expand ()
  "Expand selection to the next outer delimiter."
  (interactive)
  (when eme--thing-last-bounds
    (let ((outer (eme-thing-outer 'delimiter eme--thing-last-bounds)))
      (if outer
          (let ((bounds (plist-get outer :bounds))
                (inner (plist-get outer :inner)))
            (setq eme--thing-last-bounds bounds)
            (if (eq eme--thing-last-type 'inner)
                (eme--select-bounds inner)
              (eme--select-bounds bounds))
            (eme--thing-setup-transient-map))
        (message "No outer delimiter found")))))

;;; Transient Keymap

(defvar eme-thing-transient-map
  (let ((map (make-sparse-keymap)))
    (define-key map "t" #'eme-thing-expand)
    map)
  "Transient keymap active after thing selection.")

(defun eme--thing-setup-transient-map ()
  "Setup transient keymap for thing expansion."
  (let ((original-cursor cursor-type))
    (setq cursor-type eme-thing-cursor-type)
    (set-transient-map
     eme-thing-transient-map
     t
     (lambda ()
       (setq cursor-type original-cursor)
       (setq eme--thing-last-type nil)
       (setq eme--thing-last-bounds nil)))))

;;; Explicit Delimiter Specification

(defun eme-thing-inner-char (char)
  "Select inner of delimiter specified by CHAR."
  (interactive "cInner of: ")
  (let ((result (eme-thing-bounds 'delimiter char)))
    (if result
        (let ((inner (plist-get result :inner))
              (bounds (plist-get result :bounds)))
          (when inner
            (setq eme--thing-last-type 'inner)
            (setq eme--thing-last-bounds bounds)
            (eme--select-bounds inner)
            (eme--thing-setup-transient-map)))
      (message "No enclosing %c found" char))))

(defun eme-thing-bounds-char (char)
  "Select bounds of delimiter specified by CHAR."
  (interactive "cBounds of: ")
  (let ((result (eme-thing-bounds 'delimiter char)))
    (if result
        (let ((bounds (plist-get result :bounds)))
          (setq eme--thing-last-type 'bounds)
          (setq eme--thing-last-bounds bounds)
          (eme--select-bounds bounds)
          (eme--thing-setup-transient-map))
      (message "No enclosing %c found" char))))

;;; Thing Keymap

(defvar eme-thing-map
  (let ((map (make-sparse-keymap)))
    (define-key map "i" #'eme-thing-inner)
    (define-key map "b" #'eme-thing-bounds-select)
    ;; Explicit delimiter specification
    (define-key map "(" #'eme-thing-inner-char)
    (define-key map ")" #'eme-thing-inner-char)
    (define-key map "[" #'eme-thing-inner-char)
    (define-key map "]" #'eme-thing-inner-char)
    (define-key map "{" #'eme-thing-inner-char)
    (define-key map "}" #'eme-thing-inner-char)
    (define-key map "<" #'eme-thing-inner-char)
    (define-key map ">" #'eme-thing-inner-char)
    (define-key map "\"" #'eme-thing-inner-char)
    (define-key map "'" #'eme-thing-inner-char)
    (define-key map "`" #'eme-thing-inner-char)
    map)
  "Keymap for thing selection.")

(defun eme-thing-prefix ()
  "Enter thing selection mode.
Press i for inner, b for bounds, or a delimiter character."
  (interactive)
  (setq cursor-type eme-thing-cursor-type)
  (let ((original-cursor eme-selection-cursor-type))
    (set-transient-map
     eme-thing-map
     nil
     (lambda ()
       (setq cursor-type original-cursor)))))

(provide 'eme-thing)

;;; eme-thing.el ends here
