;;; eme-core-test.el --- Tests for eme-core -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for eme-core.el

;;; Code:

(require 'buttercup)
(require 'eme-core)

(describe "eme-core"

  (describe "customization variables"

    (it "has default selection cursor type as box"
      (expect eme-selection-cursor-type :to-equal 'box))

    (it "has default insert cursor type as bar"
      (expect eme-insert-cursor-type :to-equal 'bar))

    (it "has insert-start-modes list"
      (expect eme-insert-start-modes :to-be-truthy)))

  (describe "hooks"

    (it "defines selection mode enter hook"
      (expect (boundp 'eme-selection-mode-enter-hook) :to-be-truthy))

    (it "defines selection mode exit hook"
      (expect (boundp 'eme-selection-mode-exit-hook) :to-be-truthy))

    (it "defines insert mode enter hook"
      (expect (boundp 'eme-insert-mode-enter-hook) :to-be-truthy))

    (it "defines insert mode exit hook"
      (expect (boundp 'eme-insert-mode-exit-hook) :to-be-truthy)))

  (describe "keymaps"

    (it "defines selection mode keymap"
      (expect (keymapp eme-selection-mode-map) :to-be-truthy))

    (it "defines insert mode keymap"
      (expect (keymapp eme-insert-mode-map) :to-be-truthy))

    (it "insert mode map has C-g binding"
      (expect (lookup-key eme-insert-mode-map (kbd "C-g"))
              :to-equal #'eme-exit-insert-mode))

    (it "insert mode map has ESC binding"
      (expect (lookup-key eme-insert-mode-map (kbd "<escape>"))
              :to-equal #'eme-exit-insert-mode)))

  (describe "mode transitions"

    (before-each
      (with-current-buffer (get-buffer-create "*test-buffer*")
        (eme-local-mode -1)
        (setq eme-selection-mode nil)
        (setq eme-insert-mode nil)))

    (after-each
      (when (get-buffer "*test-buffer*")
        (kill-buffer "*test-buffer*")))

    (it "starts in selection mode when enabled"
      (with-current-buffer "*test-buffer*"
        (eme-local-mode 1)
        (expect eme-selection-mode :to-be-truthy)
        (expect eme-insert-mode :not :to-be-truthy)))

    (it "transitions from selection to insert mode"
      (with-current-buffer "*test-buffer*"
        (eme-local-mode 1)
        (eme-enter-insert-mode)
        (expect eme-insert-mode :to-be-truthy)
        (expect eme-selection-mode :not :to-be-truthy)))

    (it "transitions from insert to selection mode"
      (with-current-buffer "*test-buffer*"
        (eme-local-mode 1)
        (eme-enter-insert-mode)
        (eme-exit-insert-mode)
        (expect eme-selection-mode :to-be-truthy)
        (expect eme-insert-mode :not :to-be-truthy)))

    (it "runs enter hooks on mode entry"
      (with-current-buffer "*test-buffer*"
        (let ((hook-called nil))
          (add-hook 'eme-selection-mode-enter-hook
                    (lambda () (setq hook-called t)))
          (eme-local-mode 1)
          (expect hook-called :to-be-truthy)
          (remove-hook 'eme-selection-mode-enter-hook
                       (lambda () (setq hook-called t))))))

    (it "runs exit hooks on mode exit"
      (with-current-buffer "*test-buffer*"
        (let ((hook-called nil))
          (eme-local-mode 1)
          (add-hook 'eme-selection-mode-exit-hook
                    (lambda () (setq hook-called t)))
          (eme-enter-insert-mode)
          (expect hook-called :to-be-truthy)))))

  (describe "mode-line indicator"

    (before-each
      (with-current-buffer (get-buffer-create "*test-buffer*")
        (eme-local-mode -1)))

    (after-each
      (when (get-buffer "*test-buffer*")
        (kill-buffer "*test-buffer*")))

    (it "shows [S] in selection mode"
      (with-current-buffer "*test-buffer*"
        (eme-local-mode 1)
        (expect eme--mode-indicator :to-equal "[S]")))

    (it "shows [I] in insert mode"
      (with-current-buffer "*test-buffer*"
        (eme-local-mode 1)
        (eme-enter-insert-mode)
        (expect eme--mode-indicator :to-equal "[I]"))))

  (describe "cursor type"

    (before-each
      (with-current-buffer (get-buffer-create "*test-buffer*")
        (eme-local-mode -1)))

    (after-each
      (when (get-buffer "*test-buffer*")
        (kill-buffer "*test-buffer*")))

    (it "sets cursor to selection type in selection mode"
      (with-current-buffer "*test-buffer*"
        (eme-local-mode 1)
        (expect cursor-type :to-equal eme-selection-cursor-type)))

    (it "sets cursor to insert type in insert mode"
      (with-current-buffer "*test-buffer*"
        (eme-local-mode 1)
        (eme-enter-insert-mode)
        (expect cursor-type :to-equal eme-insert-cursor-type))))

  ;; Note: excluded-modes feature not yet implemented

  (describe "emulation mode map alists"

    (it "registers keymaps in emulation-mode-map-alists"
      (eme--setup-emulation-mode-map-alist)
      (expect (memq 'eme--emulation-mode-map-alist emulation-mode-map-alists)
              :to-be-truthy)
      (eme--teardown-emulation-mode-map-alist))

    (it "removes keymaps from emulation-mode-map-alists"
      (eme--setup-emulation-mode-map-alist)
      (eme--teardown-emulation-mode-map-alist)
      (expect (memq 'eme--emulation-mode-map-alist emulation-mode-map-alists)
              :not :to-be-truthy))))

(provide 'eme-core-test)

;;; eme-core-test.el ends here
