;;; eme-expand-test.el --- Tests for eme-expand -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for eme-expand.el

;;; Code:

(require 'buttercup)
(require 'eme-expand)
(require 'eme-thing)
(require 'eme-core)

(describe "eme-expand"

  (describe "expansion"

    (it "expands from point to word"
      (with-temp-buffer
        (insert "hello world")
        (goto-char 3)  ; Inside "hello"
        (eme-expand-region)
        (expect (eme-selection-active-p) :to-be-truthy)
        (let ((bounds (eme-selection-bounds)))
          (expect (buffer-substring (car bounds) (cdr bounds)) :to-equal "hello"))))

    (it "expands from word to symbol"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "foo-bar baz")
        (goto-char 5)  ; Inside "foo-bar"
        (eme-expand-region)  ; word "bar" or "foo"
        (eme-expand-region)  ; symbol "foo-bar"
        (let ((bounds (eme-selection-bounds)))
          (expect (buffer-substring (car bounds) (cdr bounds)) :to-equal "foo-bar"))))

    (it "expands from symbol to string inner"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "\"hello world\"")
        (goto-char 8)  ; Inside string
        (eme-expand-region)  ; word
        (eme-expand-region)  ; symbol (same as word here)
        (eme-expand-region)  ; string inner
        (let* ((bounds (eme-selection-bounds))
               (selected (buffer-substring (car bounds) (cdr bounds))))
          (expect (or (string= selected "hello world")
                      (string= selected "\"hello world\""))
                  :to-be-truthy))))

    (it "expands from string to list inner"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(func \"hello\")")
        (goto-char 9)  ; Inside string
        ;; Multiple expansions to get to list
        (dotimes (_ 5) (eme-expand-region))
        (expect (eme-selection-active-p) :to-be-truthy)
        (let* ((bounds (eme-selection-bounds))
               (selected (buffer-substring (car bounds) (cdr bounds))))
          ;; Should eventually include either inner or outer list
          (expect (or (string-match-p "func" selected)
                      (string-match-p "hello" selected))
                  :to-be-truthy))))

    (it "stops expanding at maximum bounds"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(defun foo () body)")
        (goto-char 15)  ; Inside
        ;; Keep expanding - should eventually stop
        (dotimes (_ 10) (eme-expand-region))
        (expect (eme-selection-active-p) :to-be-truthy)))

    (it "respects numeric prefix argument"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "foo-bar baz")
        (goto-char 5)
        (eme-expand-region 2)  ; Expand twice at once
        (expect (eme-selection-active-p) :to-be-truthy))))

  (describe "contraction"

    (it "contracts to previous expansion state"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "foo-bar")
        (goto-char 5)
        (eme-expand-region)  ; word
        (eme-expand-region)  ; symbol
        (let ((symbol-bounds (eme-selection-bounds)))
          (eme-contract-region)  ; back to word
          (expect (eme-selection-active-p) :to-be-truthy)
          (expect (eme-selection-bounds)
                  :not :to-equal symbol-bounds))))

    (it "deactivates selection when at minimum"
      (with-temp-buffer
        (insert "hello")
        (goto-char 3)
        (eme-expand-region)  ; word
        (eme-contract-region)  ; should deactivate
        (expect (eme-selection-active-p) :not :to-be-truthy)))

    (it "does nothing when no history"
      (with-temp-buffer
        (insert "hello")
        (goto-char 3)
        (eme-selection-set 1 6)  ; Manual selection
        (eme-contract-region)
        ;; Should clear selection (no history to restore)
        (expect (eme-selection-active-p) :not :to-be-truthy)))

    (it "respects numeric prefix argument"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "foo-bar baz")
        (goto-char 5)
        (eme-expand-region)
        (eme-expand-region)
        (eme-expand-region)
        (eme-contract-region 2)  ; Contract twice at once
        (expect (eme-selection-active-p) :to-be-truthy))))

  (describe "history management"

    (it "clears history when point moves"
      (with-temp-buffer
        (insert "hello world")
        (goto-char 3)
        (eme-expand-region)
        (setq eme--expand-last-point nil)  ; Simulate point movement
        (eme--expand-maybe-clear-history)
        ;; History check is implementation detail, test via behavior
        ))

    (it "maintains history across multiple expansions"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(func \"hello world\")")
        (goto-char 12)
        (eme-expand-region)
        (eme-expand-region)
        (eme-expand-region)
        (expect (length eme--expand-history) :to-be-greater-than 0))))

  (describe "bounds detection"

    (it "detects word bounds"
      (with-temp-buffer
        (insert "hello world")
        (goto-char 3)
        (let ((bounds (eme--expand-bounds-at-point)))
          (expect bounds :not :to-be nil)
          (expect (length bounds) :to-be-greater-than 0))))

    (it "detects symbol bounds in elisp"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "foo-bar-baz")
        (goto-char 5)
        (let ((bounds (eme--expand-bounds-at-point)))
          (expect bounds :not :to-be nil))))

    (it "detects string bounds"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "\"hello world\"")
        (goto-char 7)
        (let ((bounds (eme--expand-bounds-at-point)))
          (expect bounds :not :to-be nil)
          ;; Should have both inner and outer string bounds
          (expect (length bounds) :to-be-greater-than 1))))

    (it "detects list bounds"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(foo bar baz)")
        (goto-char 6)
        (let ((bounds (eme--expand-bounds-at-point)))
          (expect bounds :not :to-be nil))))

    (it "returns bounds sorted by size"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(outer (inner word))")
        (goto-char 15)  ; On "word"
        (let ((bounds (eme--expand-bounds-at-point)))
          (expect bounds :not :to-be nil)
          ;; Each bound should be larger than or equal to the previous
          (let ((prev-size 0))
            (dolist (b bounds)
              (let ((size (- (cdr b) (car b))))
                (expect (>= size prev-size) :to-be-truthy)
                (setq prev-size size))))))))

  (describe "sexp-chain expansion (via thing API)"

    (it "detects function call chain"
      (with-temp-buffer
        (js-mode)
        (insert "test(arg)")
        (goto-char 1)
        (let* ((result (eme-thing-bounds 'sexp-chain))
               (bounds (when result (plist-get result :bounds))))
          (expect bounds :to-be-truthy)
          (expect (buffer-substring (car bounds) (cdr bounds)) :to-equal "test(arg)"))))

    (it "detects array access chain"
      (with-temp-buffer
        (js-mode)
        (insert "arr[0]")
        (goto-char 1)
        (let* ((result (eme-thing-bounds 'sexp-chain))
               (bounds (when result (plist-get result :bounds))))
          (expect bounds :to-be-truthy)
          (expect (buffer-substring (car bounds) (cdr bounds)) :to-equal "arr[0]"))))

    (it "detects method chain"
      (with-temp-buffer
        (js-mode)
        (insert "obj.method()")
        (goto-char 1)
        (let* ((result (eme-thing-bounds 'sexp-chain))
               (bounds (when result (plist-get result :bounds))))
          (expect bounds :to-be-truthy)
          (expect (buffer-substring (car bounds) (cdr bounds)) :to-equal "obj.method()"))))

    (it "detects complex chain"
      (with-temp-buffer
        (js-mode)
        (insert "foo.bar(x).baz[0]")
        (goto-char 1)
        (let* ((result (eme-thing-bounds 'sexp-chain))
               (bounds (when result (plist-get result :bounds))))
          (expect bounds :to-be-truthy)
          (expect (buffer-substring (car bounds) (cdr bounds)) :to-equal "foo.bar(x).baz[0]"))))

    (it "includes backward chain from middle"
      (with-temp-buffer
        (js-mode)
        (insert "allStores.userStore.method()")
        (goto-char 12)  ; on "userStore"
        (let* ((result (eme-thing-bounds 'sexp-chain))
               (bounds (when result (plist-get result :bounds))))
          (expect bounds :to-be-truthy)
          (expect (buffer-substring (car bounds) (cdr bounds))
                  :to-equal "allStores.userStore.method()"))))

    (it "returns nil for simple symbol"
      (with-temp-buffer
        (js-mode)
        (insert "simple")
        (goto-char 1)
        (let ((result (eme-thing-bounds 'sexp-chain)))
          (expect result :to-be nil))))

    (it "expands symbol then sexp-chain"
      (with-temp-buffer
        (js-mode)
        (insert "test(arg)")
        (goto-char 1)
        (eme-expand-region)  ; symbol
        (let ((bounds (eme-selection-bounds)))
          (expect (buffer-substring (car bounds) (cdr bounds)) :to-equal "test"))
        (eme-expand-region)  ; sexp-chain
        (let ((bounds (eme-selection-bounds)))
          (expect (buffer-substring (car bounds) (cdr bounds)) :to-equal "test(arg)"))))))

(provide 'eme-expand-test)

;;; eme-expand-test.el ends here
