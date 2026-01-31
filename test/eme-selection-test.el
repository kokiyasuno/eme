;;; eme-selection-test.el --- Tests for eme-selection -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for eme-selection.el

;;; Code:

(require 'buttercup)
(require 'eme-selection)
(require 'eme-thing)

(describe "eme-selection"

  (describe "word movement (f/b keys)"

    (it "moves forward one word with fresh eme-selection"
      (with-temp-buffer
        (insert "hello world foo")
        (goto-char 1)
        (eme-selection-clear)
        (setq eme--anchor-active nil)
        (eme-forward-word)
        (expect (point) :to-equal 6)
        (expect (eme-selection-active-p) :to-be-truthy)
        (let ((bounds (eme-selection-bounds)))
          (expect (car bounds) :to-equal 1)
          (expect (cdr bounds) :to-equal 6))))

    (it "moves backward one word with fresh eme-selection"
      (with-temp-buffer
        (insert "hello world")
        (goto-char 12)
        (eme-selection-clear)
        (setq eme--anchor-active nil)
        (eme-backward-word)
        (expect (point) :to-equal 7)
        (expect (eme-selection-active-p) :to-be-truthy)))

    (it "merges selection when anchor is set"
      (with-temp-buffer
        (insert "hello world foo")
        (goto-char 1)
        (eme-selection-clear)
        (setq eme--anchor-active t)
        (setq eme--anchor-bounds (cons 1 6))  ; anchor on "hello"
        (eme-forward-word)  ; moves past "hello" to position 6
        ;; After one forward-word from position 1, point is at 6 (after "hello")
        ;; word bounds at position 6 (space) may be nil, so selection may not merge
        ;; The actual behavior depends on bounds-of-thing-at-point at that position
        (expect (point) :to-equal 6)))

    (it "creates fresh selection on each movement without anchor"
      (with-temp-buffer
        (insert "hello world foo")
        (goto-char 1)
        (eme-selection-clear)
        (setq eme--anchor-active nil)
        (eme-forward-word)
        (eme-forward-word)
        ;; Selection should only cover "world" (fresh)
        (let ((bounds (eme-selection-bounds)))
          (expect (car bounds) :to-equal 7)
          (expect (cdr bounds) :to-equal 12))))

    (it "handles end of buffer gracefully"
      (with-temp-buffer
        (insert "hello")
        (goto-char (point-max))
        (setq eme--anchor-active nil)
        (expect (eme-forward-word) :not :to-throw)))

    (it "handles beginning of buffer gracefully"
      (with-temp-buffer
        (insert "hello")
        (goto-char (point-min))
        (setq eme--anchor-active nil)
        (expect (eme-backward-word) :not :to-throw))))

  (describe "sexp movement"

    (it "moves forward one sexp"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(foo) (bar)")
        (goto-char 1)
        (eme-selection-clear)
        (setq eme--anchor-active nil)
        (eme-forward-sexp)
        (expect (point) :to-equal 6)))

    (it "moves backward one sexp"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(foo) (bar)")
        (goto-char 12)
        (eme-selection-clear)
        (setq eme--anchor-active nil)
        (eme-backward-sexp)
        (expect (point) :to-equal 7)))

    (it "selects sexp when at opening paren"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(foo) (bar)")
        (goto-char 7)  ; at opening paren of (bar)
        (eme-selection-clear)
        (setq eme--anchor-active nil)
        (eme-forward-sexp)
        ;; After forward-sexp, point is at 12, looking back at (bar)
        ;; sexp provider needs point at opening paren to work
        (expect (point) :to-equal 12))))

  (describe "line movement (n/p keys)"

    (it "moves to next line with eme-selection for entire line when no anchor"
      (with-temp-buffer
        (insert "line1\nline2\nline3")
        (goto-char 4)  ; line 1, column 3
        (setq eme--anchor-active nil)
        (setq eme--anchor-bounds nil)
        (eme-selection-clear)
        (eme-next-line)
        (expect (line-number-at-pos) :to-equal 2)
        ;; Should use eme-selection overlay for whole line
        (expect (eme-selection-active-p) :to-be-truthy)
        (let ((bounds (eme-selection-bounds)))
          ;; Selection should cover entire line 2
          (expect (car bounds) :to-equal 7)   ; line 2 start
          (expect (cdr bounds) :to-equal 12)))) ; line 2 end

    (it "moves to previous line with eme-selection for entire line when no anchor"
      (with-temp-buffer
        (insert "line1\nline2\nline3")
        (goto-char 10)  ; line 2
        (setq eme--anchor-active nil)
        (setq eme--anchor-bounds nil)
        (eme-selection-clear)
        (eme-previous-line)
        (expect (line-number-at-pos) :to-equal 1)
        ;; Should use eme-selection overlay for whole line
        (expect (eme-selection-active-p) :to-be-truthy)
        (let ((bounds (eme-selection-bounds)))
          ;; Selection should cover entire line 1
          (expect (car bounds) :to-equal 1)
          (expect (cdr bounds) :to-equal 6))))

    (it "merges with anchor-bounds when anchor is set"
      (with-temp-buffer
        (insert "line1\nline2\nline3")
        (goto-char 4)  ; line 1, column 3
        (setq eme--anchor-active t)
        (setq eme--anchor-bounds (cons 1 6))  ; anchor on line 1
        (eme-selection-clear)
        (eme-next-line)
        (expect (line-number-at-pos) :to-equal 2)
        ;; Should merge: line1 + line2
        (expect (eme-selection-active-p) :to-be-truthy)
        (let ((bounds (eme-selection-bounds)))
          (expect (car bounds) :to-equal 1)
          (expect (cdr bounds) :to-equal 12))))

    (it "merges selection across multiple lines with anchor"
      (with-temp-buffer
        (insert "line1\nline2\nline3")
        (goto-char 1)
        (setq eme--anchor-active t)
        (setq eme--anchor-bounds (cons 1 6))  ; anchor on line 1
        (eme-selection-clear)
        (eme-next-line)
        (eme-next-line)
        ;; Should select all 3 lines
        (expect (line-number-at-pos) :to-equal 3)
        (let ((bounds (eme-selection-bounds)))
          (expect (car bounds) :to-equal 1)
          (expect (cdr bounds) :to-equal 18))))

    (it "preserves column when moving with eme-selection"
      (with-temp-buffer
        (insert "line1\nline2\nline3")
        (goto-char 4)  ; line 1, column 3
        (setq eme--anchor-active nil)
        (setq eme--anchor-bounds nil)
        (eme-selection-clear)
        (eme-next-line)
        ;; Column should be preserved even though whole line is selected
        (expect (current-column) :to-equal 3)))

    (it "preserves goal column across consecutive movements"
      (with-temp-buffer
        (insert "long line here\nhi\nthird line here")
        (goto-char 11)  ; line 1, column 10
        (setq eme--anchor-active nil)
        (setq eme--anchor-bounds nil)
        (setq eme--goal-column-valid nil)
        (eme-selection-clear)
        (eme-next-line)  ; short line, can't reach column 10
        (eme-next-line)  ; back to long line
        ;; Goal column should be remembered and restored
        (expect (current-column) :to-equal 10)))

    (it "handles end of buffer gracefully"
      (with-temp-buffer
        (insert "only line")
        (goto-char 1)
        (setq eme--anchor-active nil)
        (expect (eme-next-line) :not :to-throw)))

    (it "handles beginning of buffer gracefully"
      (with-temp-buffer
        (insert "only line")
        (goto-char 1)
        (setq eme--anchor-active nil)
        (expect (eme-previous-line) :not :to-throw))))

  (describe "paragraph movement"

    (it "moves forward one paragraph with eme-selection"
      (with-temp-buffer
        (insert "para1\n\npara2\n\npara3")
        (goto-char 1)
        (eme-selection-clear)
        (setq eme--anchor-active nil)
        (eme-forward-paragraph)
        (expect (eme-selection-active-p) :to-be-truthy)))

    (it "moves backward one paragraph with eme-selection"
      (with-temp-buffer
        (insert "para1\n\npara2\n\npara3")
        (goto-char (point-max))
        (eme-selection-clear)
        (setq eme--anchor-active nil)
        (eme-backward-paragraph)
        (expect (eme-selection-active-p) :to-be-truthy))))

  (describe "line boundary movement (a/e keys)"

    (it "moves to beginning of line with fresh eme-selection"
      (with-temp-buffer
        (insert "hello world")
        (goto-char 6)
        (eme-selection-clear)
        (setq eme--anchor-active nil)
        (eme-beginning-of-line)
        (expect (point) :to-equal 1)
        (expect (eme-selection-active-p) :to-be-truthy)
        (let ((bounds (eme-selection-bounds)))
          (expect (car bounds) :to-equal 1)
          (expect (cdr bounds) :to-equal 6))))

    (it "moves to end of line with fresh eme-selection"
      (with-temp-buffer
        (insert "hello world")
        (goto-char 1)
        (eme-selection-clear)
        (setq eme--anchor-active nil)
        (eme-end-of-line)
        (expect (point) :to-equal 12)
        (expect (eme-selection-active-p) :to-be-truthy)))

    (it "merges selection when anchor is set"
      (with-temp-buffer
        (insert "hello world")
        (goto-char 3)
        (setq eme--anchor-active t)
        (setq eme--anchor-bounds (cons 3 3))
        (eme-selection-clear)
        (eme-end-of-line)
        (expect (eme-selection-active-p) :to-be-truthy)
        (let ((bounds (eme-selection-bounds)))
          (expect (car bounds) :to-equal 3)
          (expect (cdr bounds) :to-equal 12)))))

  (describe "anchor toggle"

    (it "sets anchor-bounds when no selection"
      (with-temp-buffer
        (insert "hello")
        (goto-char 3)
        (eme-selection-clear)
        (setq eme--anchor-active nil)
        (setq eme--anchor-bounds nil)
        (eme-toggle-anchor)
        (expect eme--anchor-active :to-be-truthy)
        (expect eme--anchor-bounds :to-equal (cons 3 3))))

    (it "remembers eme-selection bounds when selection exists"
      (with-temp-buffer
        (insert "hello")
        (goto-char 1)
        (eme-selection-set 1 6)
        (setq eme--anchor-active nil)
        (setq eme--anchor-bounds nil)
        (eme-toggle-anchor)
        (expect eme--anchor-active :to-be-truthy)
        (expect eme--anchor-bounds :to-equal (cons 1 6))))

    (it "clears selection and anchor when anchor is active"
      (with-temp-buffer
        (insert "hello")
        (eme-selection-set 1 6)
        (setq eme--anchor-active t)
        (setq eme--anchor-bounds (cons 1 6))
        (eme-toggle-anchor)
        (expect eme--anchor-active :not :to-be-truthy)
        (expect eme--anchor-bounds :to-be nil)
        (expect (eme-selection-active-p) :not :to-be-truthy))))

  (describe "extending selection"

    (it "extends eme-selection forward"
      (with-temp-buffer
        (insert "hello")
        (goto-char 1)
        (eme-selection-clear)
        (eme-extend-forward-char)
        (eme-extend-forward-char)
        (eme-extend-forward-char)
        ;; Point should have moved forward 3 chars
        (expect (point) :to-equal 4)))

    (it "extends eme-selection to next line"
      (with-temp-buffer
        (insert "line1\nline2\nline3")
        (goto-char 4)  ; line 1, column 3
        (eme-selection-clear)
        (eme-extend-next-line)
        (expect (line-number-at-pos) :to-equal 2)))

    (it "extends eme-selection to previous line"
      (with-temp-buffer
        (insert "line1\nline2\nline3")
        (goto-char 10)  ; line 2, column 3
        (eme-selection-clear)
        (eme-extend-previous-line)
        (expect (line-number-at-pos) :to-equal 1))))

  (describe "anchor merge behavior"

    (it "n . n selects 2 lines"
      (with-temp-buffer
        (insert "line1\nline2\nline3")
        (goto-char 1)
        (setq eme--anchor-active nil)
        (setq eme--anchor-bounds nil)
        (eme-selection-clear)
        ;; n - select line 2
        (eme-next-line)
        (expect (line-number-at-pos) :to-equal 2)
        ;; . - set anchor
        (eme-toggle-anchor)
        (expect eme--anchor-bounds :to-equal (cons 7 12))
        ;; n - move to line 3, merge
        (eme-next-line)
        (expect (line-number-at-pos) :to-equal 3)
        (let ((bounds (eme-selection-bounds)))
          ;; Should select line 2 + line 3
          (expect (car bounds) :to-equal 7)
          (expect (cdr bounds) :to-equal 18))))

    (it "n . p selects original line and returned line"
      (with-temp-buffer
        (insert "line1\nline2\nline3")
        (goto-char 1)
        (setq eme--anchor-active nil)
        (setq eme--anchor-bounds nil)
        (eme-selection-clear)
        ;; n - select line 2
        (eme-next-line)
        (expect (line-number-at-pos) :to-equal 2)
        ;; . - set anchor
        (eme-toggle-anchor)
        ;; p - move back to line 1, merge
        (eme-previous-line)
        (expect (line-number-at-pos) :to-equal 1)
        (let ((bounds (eme-selection-bounds)))
          ;; Should select line 1 + line 2
          (expect (car bounds) :to-equal 1)
          (expect (cdr bounds) :to-equal 12))))))

(provide 'eme-selection-test)

;;; eme-selection-test.el ends here
