;;; eme-actions-test.el --- Tests for eme-actions -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for eme-actions.el

;;; Code:

(require 'buttercup)
(require 'eme-actions)

(describe "eme-actions"

  (describe "delete"

    (it "deletes selection and saves to kill-ring"
      (with-temp-buffer
        (insert "hello world")
        (goto-char 1)
        (eme-selection-set 1 6)
        (eme-delete)
        (expect (buffer-string) :to-equal " world")
        (expect (current-kill 0) :to-equal "hello")))

    (it "deletes character at point when no selection"
      (with-temp-buffer
        (insert "hello")
        (goto-char 1)
        (eme-selection-clear)
        (eme-delete)
        (expect (buffer-string) :to-equal "ello"))))

  (describe "delete-no-save"

    (it "deletes selection without saving to kill-ring"
      (with-temp-buffer
        (insert "hello world")
        (goto-char 1)
        (eme-selection-set 1 6)
        (let ((kill-ring-before (copy-sequence kill-ring)))
          (eme-delete-no-save)
          (expect (buffer-string) :to-equal " world")
          (expect kill-ring :to-equal kill-ring-before)))))

  (describe "copy"

    (it "copies selection to kill-ring"
      (with-temp-buffer
        (insert "hello world")
        (goto-char 1)
        (eme-selection-set 1 6)
        (eme-copy)
        (expect (buffer-string) :to-equal "hello world")
        (expect (current-kill 0) :to-equal "hello")))

    (it "copies current line when no selection"
      (with-temp-buffer
        (insert "hello\nworld")
        (goto-char 1)
        (eme-selection-clear)
        (eme-copy)
        (expect (current-kill 0) :to-equal "hello")))

    (it "preserves selection after copy"
      (with-temp-buffer
        (insert "hello world")
        (eme-selection-set 1 6)
        (eme-copy)
        (expect (eme-selection-active-p) :to-be-truthy)
        (let ((bounds (eme-selection-bounds)))
          (expect (car bounds) :to-equal 1)
          (expect (cdr bounds) :to-equal 6)))))

  (describe "yank"

    (it "yanks at point"
      (with-temp-buffer
        (kill-new "test")
        (insert "hello")
        (goto-char 3)
        (eme-selection-clear)
        (eme-yank)
        (expect (buffer-string) :to-equal "hetestllo")))

    (it "replaces selection when selection active"
      (with-temp-buffer
        (kill-new "NEW")
        (insert "hello")
        (goto-char 1)
        (eme-selection-set 1 6)
        (eme-yank)
        (expect (buffer-string) :to-equal "NEW"))))

  (describe "change"

    (it "deletes selection and enters insert mode"
      (with-temp-buffer
        (eme-local-mode 1)
        (insert "hello")
        (goto-char 1)
        (eme-selection-set 1 6)
        (eme-change)
        (expect (buffer-string) :to-equal "")
        (expect eme-insert-mode :to-be-truthy))))

  (describe "replace-char"

    (it "replaces selection with character"
      (with-temp-buffer
        (insert "hello")
        (goto-char 1)
        (eme-selection-set 1 6)
        (eme-replace-char ?a)
        (expect (buffer-string) :to-equal "aaaaa")))

    (it "replaces single character when no selection"
      (with-temp-buffer
        (insert "hello")
        (goto-char 1)
        (eme-selection-clear)
        (eme-replace-char ?a)
        (expect (buffer-string) :to-equal "aello"))))

  (describe "comment-toggle"

    (it "comments line"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "hello")
        (eme-selection-clear)
        (eme-comment-toggle)
        (expect (buffer-string) :to-match "^;.*hello")))

    (it "uncomments commented line"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert ";; hello")
        (eme-selection-clear)
        (eme-comment-toggle)
        (expect (buffer-string) :to-equal "hello"))))

  (describe "indent"

    (it "increases indent"
      (with-temp-buffer
        (insert "hello")
        (eme-selection-clear)
        (eme-indent-increase)
        (expect (buffer-string) :to-match "^\\s-+hello")))

    (it "decreases indent"
      (with-temp-buffer
        (insert "    hello")
        (eme-selection-clear)
        (eme-indent-decrease)
        (expect (buffer-string) :to-equal "hello"))))

  (describe "join-lines"

    (it "joins current line with next"
      (with-temp-buffer
        (insert "hello\nworld")
        (goto-char 1)
        (eme-selection-clear)
        (eme-join-lines)
        (expect (buffer-string) :to-equal "hello world"))))

  (describe "open-line-below"

    (it "opens line below and enters insert mode"
      (with-temp-buffer
        (eme-local-mode 1)
        (insert "hello")
        (goto-char 1)
        (eme-open-line-below)
        ;; Should have inserted a newline
        (expect (string-match-p "\n" (buffer-string)) :to-be-truthy)
        (expect eme-insert-mode :to-be-truthy))))

  (describe "open-line-above"

    (it "opens line above and enters insert mode"
      (with-temp-buffer
        (eme-local-mode 1)
        (insert "hello")
        (goto-char 1)
        (eme-open-line-above)
        (expect (count-lines (point-min) (point-max)) :to-equal 2)
        (expect eme-insert-mode :to-be-truthy))))

  (describe "duplicate"

    (it "duplicates selection"
      (with-temp-buffer
        (insert "hello")
        (goto-char 1)
        (eme-selection-set 1 6)
        (eme-duplicate)
        (expect (buffer-string) :to-equal "hellohello")))

    (it "duplicates current line when no selection"
      (with-temp-buffer
        (insert "hello")
        (eme-selection-clear)
        (eme-duplicate)
        (expect (buffer-string) :to-equal "hello\nhello"))))

  (describe "toggle-case"

    (it "toggles case of selection"
      (with-temp-buffer
        (insert "Hello")
        (goto-char 1)
        (eme-selection-set 1 6)
        (eme-toggle-case)
        (expect (buffer-string) :to-equal "hELLO")))

    (it "toggles case of single character when no selection"
      (with-temp-buffer
        (insert "hello")
        (goto-char 1)
        (eme-selection-clear)
        (eme-toggle-case)
        (expect (buffer-string) :to-equal "Hello"))))

  (describe "undo/redo"

    (it "clears state on undo"
      (with-temp-buffer
        (insert "hello")
        (setq eme--anchor-active t)
        (setq eme--anchor-bounds (cons 1 6))
        ;; eme-undo clears anchor state; undo itself may fail in temp buffer
        (condition-case nil
            (eme-undo)
          (error nil))
        (expect eme--anchor-active :not :to-be-truthy)
        (expect eme--anchor-bounds :to-be nil))))

  (describe "sort-lines"

    (it "sorts selected lines"
      (with-temp-buffer
        (insert "cherry\napple\nbanana")
        (goto-char 1)
        (eme-selection-set 1 (point-max))
        (eme-sort-lines)
        (expect (buffer-string) :to-equal "apple\nbanana\ncherry"))))

  (describe "reverse-lines"

    (it "reverses selected lines"
      (with-temp-buffer
        (insert "first\nsecond\nthird")
        (goto-char 1)
        (eme-selection-set 1 (point-max))
        (eme-reverse-lines)
        (expect (buffer-string) :to-equal "third\nsecond\nfirst")))))

(provide 'eme-actions-test)

;;; eme-actions-test.el ends here
