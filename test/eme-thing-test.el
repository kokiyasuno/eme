;;; eme-thing-test.el --- Tests for eme-thing -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for eme-thing.el

;;; Code:

(require 'buttercup)
(require 'eme-thing)

(describe "eme-thing"

  (describe "thing-inner"

    (it "selects inner content of string"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "\"hello\"")
        (goto-char 4)  ; Inside
        (eme-thing-inner)
        (expect (eme-selection-active-p) :to-be-truthy)
        (let ((bounds (eme-selection-bounds)))
          (expect (car bounds) :to-equal 2)
          (expect (cdr bounds) :to-equal 7))))

    (it "selects inner content of parens"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(foo bar)")
        (goto-char 5)
        (eme-thing-inner)
        (expect (eme-selection-active-p) :to-be-truthy)
        (let ((bounds (eme-selection-bounds)))
          (expect (car bounds) :to-equal 2)
          (expect (cdr bounds) :to-equal 9))))

    (it "stores last bounds for expansion"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(foo bar)")
        (goto-char 5)
        (eme-thing-inner)
        (expect eme--thing-last-type :to-equal 'inner)
        (expect eme--thing-last-bounds :to-equal '(1 . 10))))

    (it "shows message when no delimiter found"
      (with-temp-buffer
        (insert "hello")
        (goto-char 3)
        (expect (eme-thing-inner) :not :to-throw))))

  (describe "thing-bounds-select"

    (it "selects including delimiters for string"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "\"hello\"")
        (goto-char 4)
        (eme-thing-bounds-select)
        (expect (eme-selection-active-p) :to-be-truthy)
        (let ((bounds (eme-selection-bounds)))
          (expect (car bounds) :to-equal 1)
          (expect (cdr bounds) :to-equal 8))))

    (it "selects including delimiters for parens"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(foo bar)")
        (goto-char 5)
        (eme-thing-bounds-select)
        (expect (eme-selection-active-p) :to-be-truthy)
        (let ((bounds (eme-selection-bounds)))
          (expect (car bounds) :to-equal 1)
          (expect (cdr bounds) :to-equal 10))))

    (it "stores last bounds for expansion"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(foo bar)")
        (goto-char 5)
        (eme-thing-bounds-select)
        (expect eme--thing-last-type :to-equal 'bounds)
        (expect eme--thing-last-bounds :to-equal '(1 . 10)))))

  (describe "thing-expand"

    (it "expands to outer delimiter"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(outer (inner) more)")
        (goto-char 10)  ; Inside inner
        (eme-thing-bounds-select)
        ;; Now expand to outer
        (eme-thing-expand)
        (expect eme--thing-last-bounds :to-equal '(1 . 21))))

    (it "maintains inner/bounds type when expanding"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(outer (inner) more)")
        (goto-char 10)
        (eme-thing-inner)
        (let ((original-type eme--thing-last-type))
          (eme-thing-expand)
          (expect eme--thing-last-type :to-equal original-type)))))

  (describe "thing-map keymap"

    (it "has i bound to inner"
      (expect (lookup-key eme-thing-map "i") :to-equal #'eme-thing-inner))

    (it "has b bound to bounds-select"
      (expect (lookup-key eme-thing-map "b") :to-equal #'eme-thing-bounds-select))

    (it "has delimiter keys bound"
      (expect (lookup-key eme-thing-map "(") :to-equal #'eme-thing-inner-char)
      (expect (lookup-key eme-thing-map ")") :to-equal #'eme-thing-inner-char)
      (expect (lookup-key eme-thing-map "[") :to-equal #'eme-thing-inner-char)
      (expect (lookup-key eme-thing-map "\"") :to-equal #'eme-thing-inner-char)))

  ;; Thing provider system tests
  (describe "eme-thing-bounds"

    (describe "word provider"
      (it "detects word at point"
        (with-temp-buffer
          (insert "hello world")
          (goto-char 3)  ; inside "hello"
          (let* ((result (eme-thing-bounds 'word))
                 (result-type (plist-get result :type))
                 (result-bounds (plist-get result :bounds)))
            (expect result :not :to-be nil)
            (expect result-type :to-equal 'word)
            (expect result-bounds :to-equal '(1 . 6)))))

      (it "returns nil when not on word"
        (with-temp-buffer
          (insert "   ")
          (goto-char 2)
          (let ((result (eme-thing-bounds 'word)))
            (expect result :to-be nil)))))

    (describe "symbol provider"
      (it "detects symbol at point"
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "foo-bar-baz")
          (goto-char 5)
          (let* ((result (eme-thing-bounds 'symbol))
                 (result-type (plist-get result :type))
                 (result-bounds (plist-get result :bounds)))
            (expect result :not :to-be nil)
            (expect result-type :to-equal 'symbol)
            (expect result-bounds :to-equal '(1 . 12))))))

    (describe "sexp provider"
      (it "detects sexp when at opening paren"
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "(foo bar)")
          (goto-char 1)  ; at opening paren
          (let* ((result (eme-thing-bounds 'sexp))
                 (result-type (plist-get result :type)))
            (expect result :not :to-be nil)
            (expect result-type :to-equal 'sexp)))))

    (describe "line provider"
      (it "detects current line bounds"
        (with-temp-buffer
          (insert "line1\nline2\nline3")
          (goto-char 8)  ; inside "line2"
          (let* ((result (eme-thing-bounds 'line))
                 (result-type (plist-get result :type))
                 (result-bounds (plist-get result :bounds)))
            (expect result :not :to-be nil)
            (expect result-type :to-equal 'line)
            (expect result-bounds :to-equal '(7 . 12))))))

    (describe "sentence provider"
      (it "detects sentence at point"
        (with-temp-buffer
          (insert "First sentence. Second sentence.")
          (goto-char 5)  ; inside first sentence
          (let* ((result (eme-thing-bounds 'sentence))
                 (result-type (plist-get result :type)))
            (expect result :not :to-be nil)
            (expect result-type :to-equal 'sentence)))))

    (describe "paragraph provider"
      (it "detects paragraph at point"
        (with-temp-buffer
          (insert "Para 1.\n\nPara 2.")
          (goto-char 3)  ; inside first paragraph
          (let* ((result (eme-thing-bounds 'paragraph))
                 (result-type (plist-get result :type)))
            (expect result :not :to-be nil)
            (expect result-type :to-equal 'paragraph)))))

    (describe "defun provider"
      (it "detects defun at point"
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "(defun foo ()\n  \"Doc.\"\n  (body))")
          (goto-char 10)  ; inside defun
          (let* ((result (eme-thing-bounds 'defun))
                 (result-type (plist-get result :type)))
            (expect result :not :to-be nil)
            (expect result-type :to-equal 'defun)))))

    (describe "string provider"
      (it "detects string at point"
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "\"hello world\"")
          (goto-char 7)  ; inside string
          (let* ((result (eme-thing-bounds 'string))
                 (result-type (plist-get result :type))
                 (result-inner (plist-get result :inner))
                 (result-char (plist-get result :char)))
            (expect result :not :to-be nil)
            (expect result-type :to-equal 'string)
            (expect result-inner :to-equal '(2 . 13))
            (expect result-char :to-equal ?\")))))

    (describe "list provider"
      (it "detects list at point"
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "(foo bar)")
          (goto-char 5)  ; inside list
          (let* ((result (eme-thing-bounds 'list))
                 (result-type (plist-get result :type))
                 (result-inner (plist-get result :inner))
                 (result-char (plist-get result :char)))
            (expect result :not :to-be nil)
            (expect result-type :to-equal 'list)
            (expect result-inner :to-equal '(2 . 9))
            (expect result-char :to-equal ?\()))))

    (describe "delimiter provider"
      (it "detects nearest delimiter in string"
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "(foo \"bar\")")
          (goto-char 8)  ; inside string
          (let ((result (eme-thing-bounds 'delimiter)))
            (expect result :not :to-be nil)
            ;; Auto-detect should find string first
            (expect (plist-get result :char) :to-equal ?\"))))

      (it "finds specific delimiter by char"
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "(foo [bar])")
          (goto-char 8)  ; inside brackets
          (let ((result (eme-thing-bounds 'delimiter ?\()))
            (expect result :not :to-be nil)
            ;; Should find the outer parens
            (expect (plist-get result :char) :to-equal ?\())))))

  (describe "eme-thing-all-bounds"

    (it "returns multiple bounds sorted by size"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "foo")
        (goto-char 2)  ; inside "foo"
        (let ((results (eme-thing-all-bounds)))
          (expect results :not :to-be nil)
          ;; Should have at least word, symbol
          (expect (length results) :to-be-greater-than 0)
          ;; Should be sorted by size (smallest first)
          (let ((prev-size 0))
            (dolist (r results)
              (let* ((bounds (plist-get r :bounds))
                     (size (- (cdr bounds) (car bounds))))
                (expect size :not :to-be-less-than prev-size)
                (setq prev-size size)))))))

    (it "removes duplicates"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "foo")
        (goto-char 2)
        (let ((results (eme-thing-all-bounds)))
          ;; Check no duplicate bounds
          (let ((bounds-list (mapcar (lambda (r) (plist-get r :bounds)) results)))
            (expect (length bounds-list) :to-equal (length (delete-dups (copy-sequence bounds-list)))))))))

  (describe "eme-thing-outer"

    (it "finds outer bounds of same type"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(outer (inner) more)")
        (goto-char 10)  ; inside inner
        (let* ((inner (eme-thing-bounds 'delimiter))
               (inner-bounds (plist-get inner :bounds))
               (outer (eme-thing-outer 'delimiter inner-bounds)))
          (expect outer :not :to-be nil)
          (expect (plist-get outer :bounds) :to-equal '(1 . 21)))))

    (it "returns nil when no outer found"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(only)")
        (goto-char 3)
        (let* ((current (eme-thing-bounds 'delimiter))
               (current-bounds (plist-get current :bounds))
               (outer (eme-thing-outer 'delimiter current-bounds)))
          (expect outer :to-be nil)))))

  (describe "internal helper functions"

    (describe "eme-thing--get-matching-char"
      (it "returns matching paren"
        (expect (eme-thing--get-matching-char ?\() :to-equal ?\)))

      (it "returns matching bracket"
        (expect (eme-thing--get-matching-char ?\[) :to-equal ?\]))

      (it "returns matching brace"
        (expect (eme-thing--get-matching-char ?\{) :to-equal ?\}))

      (it "returns matching angle bracket"
        (expect (eme-thing--get-matching-char ?\<) :to-equal ?\>))

      (it "returns same char for quotes"
        (expect (eme-thing--get-matching-char ?\") :to-equal ?\")
        (expect (eme-thing--get-matching-char ?\') :to-equal ?\')
        (expect (eme-thing--get-matching-char ?\`) :to-equal ?\`))))

  (describe "explicit delimiter specification"

    (it "thing-inner-char selects inner of specific delimiter"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(foo [bar] baz)")
        (goto-char 8)  ; inside brackets
        ;; Test the underlying eme-thing-bounds with char
        ;; [ is at pos 6, ] is at pos 10, so inner is (7 . 10)
        (let ((result (eme-thing-bounds 'delimiter ?\[)))
          (expect result :not :to-be nil)
          (expect (plist-get result :char) :to-equal ?\[)
          (expect (plist-get result :inner) :to-equal '(7 . 10)))))

    (it "finds enclosing brackets correctly"
      (with-temp-buffer
        (insert "[foo bar]")
        (goto-char 5)
        (let ((result (eme-thing-bounds 'delimiter ?\[)))
          (expect result :not :to-be nil)
          (expect (plist-get result :bounds) :to-equal '(1 . 10)))))

    (it "finds enclosing braces"
      (with-temp-buffer
        (insert "{foo bar}")
        (goto-char 5)
        (let ((result (eme-thing-bounds 'delimiter ?\{)))
          (expect result :not :to-be nil)
          (expect (plist-get result :bounds) :to-equal '(1 . 10)))))

    (it "handles nested brackets correctly"
      (with-temp-buffer
        (insert "[[inner] outer]")
        (goto-char 5)  ; Inside inner
        (let ((result (eme-thing-bounds 'delimiter ?\[)))
          (expect result :not :to-be nil)
          (expect (plist-get result :bounds) :to-equal '(2 . 9)))))))

(provide 'eme-thing-test)

;;; eme-thing-test.el ends here
