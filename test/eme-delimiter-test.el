;;; eme-delimiter-test.el --- Tests for eme-delimiter -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for eme-delimiter.el

;;; Code:

(require 'buttercup)
(require 'eme-delimiter)
(require 'eme-core)
(require 'eme-thing)

(describe "eme-delimiter"

  (describe "delimiter detection (via thing API)"

    (it "finds nearest paren"
      (with-temp-buffer
        (insert "(hello)")
        (goto-char 4)
        (let ((result (eme-thing-bounds 'delimiter)))
          (expect result :to-be-truthy)
          (expect (plist-get result :char) :to-equal ?\()
          (expect (car (plist-get result :bounds)) :to-equal 1)
          (expect (cdr (plist-get result :bounds)) :to-equal 8))))  ; includes closing

    (it "finds nearest quote"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "\"hello\"")
        (goto-char 4)
        (let ((result (eme-thing-bounds 'delimiter)))
          (expect result :to-be-truthy)
          (expect (plist-get result :char) :to-equal ?\")
          (expect (car (plist-get result :bounds)) :to-equal 1))))

    (it "prefers closest delimiter in nested context"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(\"hello\")")
        (goto-char 5)  ; inside "hello"
        (let ((result (eme-thing-bounds 'delimiter)))
          (expect result :to-be-truthy)
          ;; Should find quote as it's closest
          (expect (plist-get result :char) :to-equal ?\"))))

    (it "returns nil when no delimiter found"
      (with-temp-buffer
        (insert "hello")
        (goto-char 3)
        (let ((result (eme-thing-bounds 'delimiter)))
          (expect result :to-be nil)))))

  (describe "inner selection (m key)"

    (before-each
      (setq eme--delimiter-context nil))

    (it "selects inner content of parens"
      (with-temp-buffer
        (insert "(hello)")
        (goto-char 4)
        (eme-delimiter-inner)
        (expect (eme-selection-active-p) :to-be-truthy)
        (let ((bounds (eme-selection-bounds)))
          (expect (buffer-substring (car bounds) (cdr bounds))
                  :to-equal "hello"))))

    (it "sets delimiter context after selection"
      (with-temp-buffer
        (insert "(hello)")
        (goto-char 4)
        (eme-delimiter-inner)
        (expect eme--delimiter-context :to-be-truthy)
        (expect (car eme--delimiter-context) :to-equal ?\()))

    (it "selects inner content of quotes"
      (with-temp-buffer
        (insert "\"hello\"")
        (goto-char 4)
        (eme-delimiter-inner)
        (expect (eme-selection-active-p) :to-be-truthy)
        (let ((bounds (eme-selection-bounds)))
          (expect (buffer-substring (car bounds) (cdr bounds))
                  :to-equal "hello"))))

    (it "expands to outer delimiter on repeat"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(\"hello\")")
        (goto-char 5)
        ;; First m: select quote inner
        (eme-delimiter-inner)
        (expect (eme-selection-active-p) :to-be-truthy)
        (when (eme-selection-active-p)
          (let ((bounds (eme-selection-bounds)))
            (expect (buffer-substring (car bounds) (cdr bounds))
                    :to-equal "hello"))))))

  (describe "bounds selection (M key)"

    (before-each
      (setq eme--delimiter-context nil))

    (it "selects bounds including delimiters"
      (with-temp-buffer
        (insert "(hello)")
        (goto-char 4)
        (eme-delimiter-bounds)
        (expect (eme-selection-active-p) :to-be-truthy)
        (let ((bounds (eme-selection-bounds)))
          (expect (buffer-substring (car bounds) (cdr bounds))
                  :to-equal "(hello)"))))

    (it "does not set delimiter context"
      (with-temp-buffer
        (insert "(hello)")
        (goto-char 4)
        (eme-delimiter-bounds)
        (expect eme--delimiter-context :to-be nil)))

    (it "keeps selection when no delimiter found"
      (with-temp-buffer
        (insert "hello")
        (eme-selection-set 1 6)  ; select "hello" with eme-selection
        (eme-delimiter-bounds)
        ;; Should keep the selection
        (expect (eme-selection-active-p) :to-be-truthy)
        (let ((bounds (eme-selection-bounds)))
          (expect (buffer-substring (car bounds) (cdr bounds))
                  :to-equal "hello")))))

  (describe "delimiter delete"

    (it "deletes delimiters keeping content"
      (with-temp-buffer
        (insert "(hello)")
        (goto-char 4)
        (let ((bounds (cons 1 7)))  ; open-pos, close-pos
          (eme--delimiter-delete bounds))
        (expect (buffer-string) :to-equal "hello")))

    (it "deletes quote delimiters"
      (with-temp-buffer
        (insert "\"hello\"")
        (goto-char 4)
        (let ((bounds (cons 1 7)))
          (eme--delimiter-delete bounds))
        (expect (buffer-string) :to-equal "hello"))))

  (describe "delimiter change"

    (it "changes parens to brackets"
      (with-temp-buffer
        (insert "(hello)")
        (goto-char 4)
        (let ((bounds (cons 1 7)))
          (eme--delimiter-change bounds ?\[))
        (expect (buffer-string) :to-equal "[hello]")))

    (it "changes quotes to parens"
      (with-temp-buffer
        (insert "\"hello\"")
        (goto-char 4)
        (let ((bounds (cons 1 7)))
          (eme--delimiter-change bounds ?\())
        (expect (buffer-string) :to-equal "(hello)"))))

  (describe "wrap"

    (it "wraps selection with parens"
      (with-temp-buffer
        (insert "hello")
        (eme-selection-set 1 6)  ; select "hello"
        (eme--delimiter-wrap ?\()
        (expect (buffer-string) :to-equal "(hello)")))

    (it "wraps selection with quotes"
      (with-temp-buffer
        (insert "hello")
        (eme-selection-set 1 6)  ; select "hello"
        (eme--delimiter-wrap ?\")
        (expect (buffer-string) :to-equal "\"hello\""))))

  (describe "normalize delimiter"

    (it "normalizes closing paren to opening"
      (expect (eme--normalize-delimiter ?\)) :to-equal ?\())

    (it "normalizes closing bracket to opening"
      (expect (eme--normalize-delimiter ?\]) :to-equal ?\[))

    (it "keeps opening delimiter as is"
      (expect (eme--normalize-delimiter ?\() :to-equal ?\())

    (it "keeps quote as is"
      (expect (eme--normalize-delimiter ?\") :to-equal ?\")))

  (describe "keybindings"

    (it "m key is bound to delimiter-wrap-or-inner"
      (expect (lookup-key eme-selection-mode-map "m")
              :to-equal #'eme-delimiter-wrap-or-inner))

    (it "M key is bound to delimiter-bounds"
      (expect (lookup-key eme-selection-mode-map "M")
              :to-equal #'eme-delimiter-bounds))))

(provide 'eme-delimiter-test)

;;; eme-delimiter-test.el ends here
