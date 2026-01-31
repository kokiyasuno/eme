;;; eme-test.el --- Tests for eme -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Koki Yasuno

;;; Commentary:

;; Test suite for eme using buttercup.
;; Run with: emacs -batch -l buttercup -f buttercup-run-discover

;;; Code:

(require 'buttercup)

;; Add parent directory to load-path for package files
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../eme" dir)))

(require 'eme)

(describe "eme integration"

  (describe "package loading"

    (it "provides eme feature"
      (expect (featurep 'eme) :to-be-truthy))

    (it "provides eme-core feature"
      (expect (featurep 'eme-core) :to-be-truthy))

    (it "provides eme-selection feature"
      (expect (featurep 'eme-selection) :to-be-truthy))

    (it "provides eme-actions feature"
      (expect (featurep 'eme-actions) :to-be-truthy))

    (it "provides eme-search feature"
      (expect (featurep 'eme-search) :to-be-truthy))

    (it "provides eme-goto-view feature"
      (expect (featurep 'eme-goto-view) :to-be-truthy))

    (it "provides eme-macro-register feature"
      (expect (featurep 'eme-macro-register) :to-be-truthy))

    (it "provides eme-keybinds feature"
      (expect (featurep 'eme-keybinds) :to-be-truthy))

    (it "provides eme-delimiter feature"
      (expect (featurep 'eme-delimiter) :to-be-truthy)))

  (describe "global mode"

    (it "can enable global mode"
      (eme-mode 1)
      (expect eme-mode :to-be-truthy))

    (it "can disable global mode"
      (eme-mode 1)
      (eme-mode -1)
      (expect eme-mode :not :to-be-truthy)))

  (describe "workflow integration"

    (it "enables selection mode in new buffers"
      (eme-mode 1)
      (with-temp-buffer
        (eme-local-mode 1)
        (expect eme-selection-mode :to-be-truthy)))

    (it "completes selection -> action workflow"
      (eme-mode 1)
      (with-temp-buffer
        (eme-local-mode 1)
        (insert "hello world")
        (goto-char 1)
        ;; Move forward to select
        (eme-forward-word)
        (expect (eme-selection-active-p) :to-be-truthy)
        ;; Delete selection
        (eme-delete)
        (expect (buffer-string) :to-equal " world")
        (expect (current-kill 0) :to-equal "hello")))

    (it "handles mode transitions correctly"
      (eme-mode 1)
      (with-temp-buffer
        (eme-local-mode 1)
        (expect eme-selection-mode :to-be-truthy)
        (expect eme-insert-mode :not :to-be-truthy)
        ;; Enter insert mode
        (eme-enter-insert-mode)
        (expect eme-insert-mode :to-be-truthy)
        (expect eme-selection-mode :not :to-be-truthy)
        ;; Exit insert mode
        (eme-exit-insert-mode)
        (expect eme-selection-mode :to-be-truthy)
        (expect eme-insert-mode :not :to-be-truthy)))))

(provide 'eme-test)

;;; eme-test.el ends here
