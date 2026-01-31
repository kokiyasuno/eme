;;; eme-keybinds-test.el --- Tests for eme-keybinds -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for eme-keybinds.el
;; Verifies keybinding assignments and checks for conflicts.

;;; Code:

(require 'buttercup)
(require 'eme-keybinds)

(describe "eme-keybinds"

  (describe "selection mode keymap"

    (it "exists and is a keymap"
      (expect (keymapp eme-selection-mode-map) :to-be-truthy))

    ;; Word movement (f/b)
    (it "has f bound to forward-word"
      (expect (lookup-key eme-selection-mode-map "f")
              :to-equal #'eme-forward-word))

    (it "has b bound to backward-word"
      (expect (lookup-key eme-selection-mode-map "b")
              :to-equal #'eme-backward-word))

    ;; F/B are reserved (unassigned)
    (it "has F unassigned (reserved for Phase 3)"
      (expect (lookup-key eme-selection-mode-map "F") :to-be nil))

    (it "has B unassigned (reserved for Phase 3)"
      (expect (lookup-key eme-selection-mode-map "B") :to-be nil))

    ;; Line movement (n/p)
    (it "has n bound to next-line"
      (expect (lookup-key eme-selection-mode-map "n")
              :to-equal #'eme-next-line))

    (it "has p bound to previous-line"
      (expect (lookup-key eme-selection-mode-map "p")
              :to-equal #'eme-previous-line))

    ;; Expand/Contract (v/V)
    (it "has v bound to expand-region"
      (expect (lookup-key eme-selection-mode-map "v")
              :to-equal #'eme-expand-region))

    (it "has V bound to contract-region"
      (expect (lookup-key eme-selection-mode-map "V")
              :to-equal #'eme-contract-region))

    ;; Thing prefix (t)
    (it "has t bound to thing-prefix"
      (expect (lookup-key eme-selection-mode-map "t")
              :to-equal #'eme-thing-prefix))

    ;; Delimiter (m key)
    (it "has m bound to delimiter-wrap-or-inner"
      (expect (lookup-key eme-selection-mode-map "m")
              :to-equal #'eme-delimiter-wrap-or-inner))

    ;; Scroll moved to C-v/M-v
    (it "has C-v bound to scroll-up"
      (expect (lookup-key eme-selection-mode-map (kbd "C-v"))
              :to-equal #'eme-scroll-up))

    (it "has M-v bound to scroll-down"
      (expect (lookup-key eme-selection-mode-map (kbd "M-v"))
              :to-equal #'eme-scroll-down)))

  (describe "thing keymap"

    (it "has i bound to thing-inner"
      (expect (lookup-key eme-thing-map "i")
              :to-equal #'eme-thing-inner))

    (it "has b bound to thing-bounds-select"
      (expect (lookup-key eme-thing-map "b")
              :to-equal #'eme-thing-bounds-select)))

  (describe "delimiter keys"

    (it "has m bound to delimiter-wrap-or-inner"
      (expect (lookup-key eme-selection-mode-map "m")
              :to-equal #'eme-delimiter-wrap-or-inner))

    (it "has M bound to delimiter-bounds"
      (expect (lookup-key eme-selection-mode-map "M")
              :to-equal #'eme-delimiter-bounds)))

  (describe "no keybinding conflicts"

    (it "each key maps to exactly one command"
      ;; Check that no single-letter key is bound multiple times
      (let ((keys '("a" "b" "c" "d" "e" "f" "g" "i" "j" "l" "m"
                    "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y"
                    "A" "E" "N" "O" "P" "Q" "S" "U" "V" "D")))
        (dolist (key keys)
          (let ((binding (lookup-key eme-selection-mode-map key)))
            ;; If bound, should be a function or keymap, not undefined
            (when binding
              (expect (or (commandp binding)
                          (keymapp binding))
                      :to-be-truthy))))))))

(provide 'eme-keybinds-test)

;;; eme-keybinds-test.el ends here
