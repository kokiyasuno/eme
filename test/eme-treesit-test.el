;;; eme-treesit-test.el --- Tests for tree-sitter integration in eme-thing -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for tree-sitter integration in eme-thing.el
;; These functions are internal (prefixed with --) but tested for correctness.
;; Tests that require tree-sitter are skipped when tree-sitter is not available.

;;; Code:

(require 'buttercup)
(require 'eme-thing)

;; Helper to check if tree-sitter JavaScript is available
(defun eme-test--ts-js-available-p ()
  "Return non-nil if tree-sitter JavaScript support is available."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)
       (fboundp 'treesit-language-available-p)
       (treesit-language-available-p 'javascript)))

(describe "eme-thing tree-sitter integration"

  (describe "availability"

    (it "returns nil in non-ts-mode"
      (with-temp-buffer
        (fundamental-mode)
        (expect (eme-thing--treesit-available-p) :not :to-be-truthy))))

  (describe "expansion bounds"

    (it "returns nil in non-ts-mode"
      (with-temp-buffer
        (fundamental-mode)
        (insert "foo.bar()")
        (goto-char 1)
        (expect (eme-thing--treesit-expansion-bounds) :to-be nil)))))

;; Only run tree-sitter dependent tests when JS tree-sitter is available
(when (eme-test--ts-js-available-p)
  (describe "eme-thing tree-sitter integration (JS)"

    (describe "availability"

      (it "detects tree-sitter in ts-mode"
        (with-temp-buffer
          (js-ts-mode)
          (expect (eme-thing--treesit-available-p) :to-be-truthy))))

    (describe "expansion bounds"

      (it "returns AST-based bounds in ts-mode"
        (with-temp-buffer
          (js-ts-mode)
          (insert "foo.bar()")
          (goto-char 1)
          (let ((bounds (eme-thing--treesit-expansion-bounds)))
            (expect bounds :to-be-truthy)
            (expect (length bounds) :to-be-greater-than 0)))))

    (describe "delimiter detection"

      (it "finds string delimiter in ts-mode"
        (with-temp-buffer
          (js-ts-mode)
          (insert "const x = \"hello\"")
          (goto-char 14)  ; inside string
          (let ((delims (eme-thing--treesit-find-enclosing-delimiters)))
            (expect delims :to-be-truthy)
            (expect (car (car delims)) :to-equal ?\"))))

      (it "finds bracket delimiter in ts-mode"
        (with-temp-buffer
          (js-ts-mode)
          (insert "function foo() { body }")
          (goto-char 20)  ; inside block
          (let ((delims (eme-thing--treesit-find-enclosing-delimiters)))
            (expect delims :to-be-truthy)
            (expect (car (car delims)) :to-equal ?\{))))

      (it "finds multiple delimiters from inner to outer"
        (with-temp-buffer
          (js-ts-mode)
          (insert "function foo() { const x = \"hello\" }")
          (goto-char 32)  ; inside string
          (let ((delims (eme-thing--treesit-find-enclosing-delimiters)))
            (expect delims :to-be-truthy)
            (expect (length delims) :to-be-greater-than 1)
            ;; First should be string, second should be block
            (expect (car (car delims)) :to-equal ?\")
            (expect (car (cadr delims)) :to-equal ?\{)))))

    (describe "parent nodes"

      (it "returns nodes from smallest to largest"
        (with-temp-buffer
          (js-ts-mode)
          (insert "foo.bar()")
          (goto-char 1)
          (let ((nodes (eme-thing--treesit-parent-nodes)))
            (expect nodes :to-be-truthy)
            (expect (length nodes) :to-be-greater-than 1)))))))

(provide 'eme-treesit-test)

;;; eme-treesit-test.el ends here
