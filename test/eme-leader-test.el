;;; eme-leader-test.el --- Tests for eme-leader -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for eme-leader.el

;;; Code:

(require 'buttercup)
(require 'eme-leader)
(require 'eme-keybinds)

(describe "eme-leader"

  (describe "customization variables"

    (it "defines leader key for selection mode"
      (expect eme-leader-key :to-be-truthy))

    (it "defines leader key for insert mode"
      (expect eme-leader-key-insert :to-be-truthy))

    (it "defines prefix alist"
      (expect eme-leader-prefix-alist :to-be-truthy)
      (expect (assoc "x" eme-leader-prefix-alist) :to-be-truthy)
      (expect (assoc "h" eme-leader-prefix-alist) :to-be-truthy)))

  (describe "user map"

    (it "defines eme-user-map"
      (expect (keymapp eme-user-map) :to-be-truthy)))

  (describe "keybindings"

    (it "binds SPC to eme-leader-start in selection mode"
      (expect (lookup-key eme-selection-mode-map (kbd "SPC"))
              :to-equal #'eme-leader-start))

    (it "binds M-SPC to eme-leader-start in insert mode"
      (expect (lookup-key eme-insert-mode-map (kbd "M-SPC"))
              :to-equal #'eme-leader-start)))

  (describe "state variables"

    (before-each
      (with-current-buffer (get-buffer-create "*test-leader*")
        (setq eme-leader--add-control nil)
        (setq eme-leader--current-prefix nil)))

    (after-each
      (when (get-buffer "*test-leader*")
        (kill-buffer "*test-leader*")))

    (it "has add-control variable"
      (with-current-buffer "*test-leader*"
        (expect eme-leader--add-control :to-be nil)))

    (it "has current-prefix variable"
      (with-current-buffer "*test-leader*"
        (expect eme-leader--current-prefix :to-be nil)))))

(describe "eme-insert-start-modes"

  (describe "detection"

    (it "detects dired-mode as insert-start"
      (with-temp-buffer
        (dired-mode)
        (expect (eme--insert-start-mode-p) :to-be-truthy)))

    (it "detects emacs-lisp-mode as not insert-start"
      (with-temp-buffer
        (emacs-lisp-mode)
        (expect (eme--insert-start-mode-p) :not :to-be-truthy)))

    (it "detects help-mode as insert-start"
      (with-temp-buffer
        (help-mode)
        (expect (eme--insert-start-mode-p) :to-be-truthy))))

  (describe "mode start behavior"

    (before-each
      (with-current-buffer (get-buffer-create "*test-insert-start*")
        (emacs-lisp-mode)
        (eme-local-mode -1)))

    (after-each
      (when (get-buffer "*test-insert-start*")
        (kill-buffer "*test-insert-start*")))

    (it "starts in selection mode for normal buffers"
      (with-current-buffer "*test-insert-start*"
        (emacs-lisp-mode)
        (eme-local-mode 1)
        (expect eme-selection-mode :to-be-truthy)
        (expect eme-insert-mode :not :to-be-truthy)))

    (it "starts in insert mode for insert-start-modes"
      (with-temp-buffer
        (help-mode)
        (eme-local-mode 1)
        (expect eme-insert-mode :to-be-truthy)
        (expect eme-selection-mode :not :to-be-truthy)))))

(provide 'eme-leader-test)

;;; eme-leader-test.el ends here
