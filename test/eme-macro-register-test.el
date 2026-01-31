;;; eme-macro-register-test.el --- Tests for eme-macro-register -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for eme-macro-register.el

;;; Code:

(require 'buttercup)
(require 'eme-macro-register)

(describe "eme-macro-register"

  (describe "macro recording"

    (it "has toggle record function"
      (expect (fboundp 'eme-macro-toggle-record) :to-be-truthy))

    (it "has play function"
      (expect (fboundp 'eme-macro-play) :to-be-truthy)))

  (describe "register operations"

    (before-each
      (set-register ?t nil))

    (it "saves position to register"
      (with-temp-buffer
        (insert "hello world")
        (goto-char 5)
        (set-register ?t (point-marker))
        (let ((val (get-register ?t)))
          (expect (markerp val) :to-be-truthy)
          (expect (marker-position val) :to-equal 5))))

    (it "saves text to register"
      (with-temp-buffer
        (insert "hello world")
        (copy-to-register ?t 1 6)
        (let ((val (get-register ?t)))
          (expect (stringp val) :to-be-truthy)
          (expect val :to-equal "hello"))))

    (it "inserts text from register"
      (with-temp-buffer
        (set-register ?t "inserted")
        (eme-register-insert ?t)
        (expect (buffer-string) :to-equal "inserted")))

    (it "handles empty register gracefully"
      (with-temp-buffer
        (set-register ?t nil)
        (expect (eme-register-insert ?t) :not :to-throw))))

  (describe "register prefix map"

    (it "defines prefix map"
      (expect (keymapp eme-register-prefix-map) :to-be-truthy)))

  (describe "mode-line indicator"

    (it "has macro mode-line function"
      (expect (fboundp 'eme--macro-mode-line) :to-be-truthy))

    (it "returns empty string when not recording"
      (let ((defining-kbd-macro nil))
        (expect (eme--macro-mode-line) :to-equal "")))))

(provide 'eme-macro-register-test)

;;; eme-macro-register-test.el ends here
