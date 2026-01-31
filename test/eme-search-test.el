;;; eme-search-test.el --- Tests for eme-search -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for eme-search.el

;;; Code:

(require 'buttercup)
(require 'eme-search)
(require 'eme-core)

(describe "eme-search"

  (describe "search-next"

    (it "finds next occurrence"
      (with-temp-buffer
        (insert "hello hello hello")
        (goto-char 1)
        (setq eme--search-last-string "hello")
        (setq eme--search-last-regexp-p nil)
        (eme-search-next)
        (expect (point) :to-equal 12)
        (expect (eme-selection-active-p) :to-be-truthy)))

    (it "wraps to beginning when at end"
      (with-temp-buffer
        (insert "hello world hello")
        (goto-char 13)
        (setq eme--search-last-string "hello")
        (setq eme--search-last-regexp-p nil)
        (eme-search-next)
        ;; Should wrap to first occurrence
        (expect (point) :to-equal 6)))

    (it "handles no match gracefully"
      (with-temp-buffer
        (insert "hello world")
        (goto-char 1)
        (setq eme--search-last-string "xyz")
        (setq eme--search-last-regexp-p nil)
        (expect (eme-search-next) :not :to-throw)
        (expect (point) :to-equal 1))))

  (describe "search-previous"

    (it "finds previous occurrence"
      (with-temp-buffer
        (insert "hello hello hello")
        (goto-char (point-max))
        (setq eme--search-last-string "hello")
        (setq eme--search-last-regexp-p nil)
        (eme-search-previous)
        (expect (point) :to-equal 13)
        (expect (eme-selection-active-p) :to-be-truthy)))

    (it "wraps to end when at beginning"
      (with-temp-buffer
        (insert "hello world hello")
        (goto-char 1)
        (setq eme--search-last-string "hello")
        (setq eme--search-last-regexp-p nil)
        (eme-search-previous)
        ;; Should wrap to last occurrence
        (expect (point) :to-equal 13))))

  (describe "regexp search"

    (it "supports regexp patterns"
      (with-temp-buffer
        (insert "cat bat rat")
        (goto-char 1)
        (setq eme--search-last-string ".at")
        (setq eme--search-last-regexp-p t)
        (eme-search-next)
        ;; Should find "bat" at position 5
        (expect (point) :to-equal 8))))

  (describe "no previous search"

    (it "shows message when no previous search"
      (with-temp-buffer
        (insert "hello")
        (setq eme--search-last-string nil)
        (expect (eme-search-next) :not :to-throw)))))

(provide 'eme-search-test)

;;; eme-search-test.el ends here
