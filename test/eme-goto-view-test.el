;;; eme-goto-view-test.el --- Tests for eme-goto-view -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for eme-goto-view.el

;;; Code:

(require 'buttercup)
(require 'eme-goto-view)

(describe "eme-goto-view"

  (describe "goto-line"

    (it "goes to specified line"
      (with-temp-buffer
        (insert "line1\nline2\nline3\nline4\nline5")
        (eme-goto-line 3)
        (expect (line-number-at-pos) :to-equal 3)))

    (it "goes to first line"
      (with-temp-buffer
        (insert "line1\nline2\nline3")
        (goto-char (point-max))
        (eme-goto-line 1)
        (expect (line-number-at-pos) :to-equal 1)))

    (it "handles line beyond buffer"
      (with-temp-buffer
        (insert "line1\nline2")
        (eme-goto-line 100)
        ;; Should be at last line (or close to it)
        (expect (point) :to-equal (point-max)))))

  (describe "scroll commands"

    (it "scroll-up handles end of buffer"
      (with-temp-buffer
        (insert "hello")
        (goto-char (point-max))
        (expect (eme-scroll-up) :not :to-throw)))

    (it "scroll-down handles beginning of buffer"
      (with-temp-buffer
        (insert "hello")
        (goto-char (point-min))
        (expect (eme-scroll-down) :not :to-throw))))

  ;; Note: recenter tests require a window, skip in batch mode
  (unless noninteractive
    (describe "recenter"

      (it "calls recenter-top-bottom"
        (with-temp-buffer
          (insert (make-string 1000 ?x))
          (goto-char 500)
          (expect (eme-recenter) :not :to-throw))))))

(provide 'eme-goto-view-test)

;;; eme-goto-view-test.el ends here
