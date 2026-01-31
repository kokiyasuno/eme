;;; eme-core.el --- Core module for eme -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Koki Yasuno

;;; Commentary:

;; Core definitions for eme:
;; - Customization group and variables
;; - Mode-line indicator
;; - Mode transition hooks
;; - Keymap infrastructure
;; - Minor mode definitions (Selection Mode, Insert Mode)
;; - Global and local mode definitions

;;; Code:

;;; Customization Group

(defgroup eme nil
  "Modal editing for Emacs with Emacs keybindings."
  :group 'editing
  :prefix "eme-")

;;; Customization Variables

(defcustom eme-selection-cursor-type 'box
  "Cursor type in Selection Mode."
  :type '(choice (const :tag "Box" box)
                 (const :tag "Bar" bar)
                 (const :tag "Horizontal bar" hbar))
  :group 'eme)

(defcustom eme-insert-cursor-type 'bar
  "Cursor type in Insert Mode."
  :type '(choice (const :tag "Box" box)
                 (const :tag "Bar" bar)
                 (const :tag "Horizontal bar" hbar))
  :group 'eme)

(defcustom eme-insert-start-modes
  '(special-mode
    dired-mode
    magit-mode
    magit-status-mode
    magit-log-mode
    magit-diff-mode
    vterm-mode
    term-mode
    eshell-mode
    Info-mode
    help-mode
    compilation-mode
    grep-mode
    org-agenda-mode)
  "List of major modes that start in Insert Mode when eme is enabled.
These modes retain their native keybindings while allowing access to eme
features through the Leader Key (M-SPC).  Modes derived from these are
also included automatically via `derived-mode-p'."
  :type '(repeat symbol)
  :group 'eme)

(defcustom eme-delimiter-chars '(?\" ?\' ?\( ?\) ?\[ ?\] ?\{ ?\} ?\` ?\< ?\>)
  "Characters recognized as delimiters for `m` operations.
Note: Adding action keys (d, w, c, etc.) here is meaningless
as they will be interpreted as actions, not delimiters."
  :type '(repeat character)
  :group 'eme)

;;; Hooks

(defvar eme-selection-mode-enter-hook nil
  "Hook run when entering Selection Mode.")

(defvar eme-selection-mode-exit-hook nil
  "Hook run when exiting Selection Mode.")

(defvar eme-insert-mode-enter-hook nil
  "Hook run when entering Insert Mode.")

(defvar eme-insert-mode-exit-hook nil
  "Hook run when exiting Insert Mode.")

;; Forward declarations for mode variables (defined by define-minor-mode below)
(defvar eme-selection-mode nil "Non-nil when Selection Mode is active.")
(defvar eme-insert-mode nil "Non-nil when Insert Mode is active.")

;;; Keymaps

(defvar eme-selection-mode-map
  (make-sparse-keymap)
  "Keymap for Selection Mode.")

(defvar eme-insert-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-g") #'eme-exit-insert-mode)
    (define-key map (kbd "<escape>") #'eme-exit-insert-mode)
    map)
  "Keymap for Insert Mode.
Only contains bindings to exit Insert Mode.")

;;; Emulation Mode Map Alists Registration

(defvar eme--emulation-mode-map-alist nil
  "Alist for `emulation-mode-map-alists'.
Ensures modal keymaps have highest priority.")

(defun eme--setup-emulation-mode-map-alist ()
  "Set up the emulation mode map alist for proper keymap priority."
  (setq eme--emulation-mode-map-alist
        `((eme-selection-mode . ,eme-selection-mode-map)
          (eme-insert-mode . ,eme-insert-mode-map)))
  (unless (memq 'eme--emulation-mode-map-alist emulation-mode-map-alists)
    (push 'eme--emulation-mode-map-alist emulation-mode-map-alists)))

(defun eme--teardown-emulation-mode-map-alist ()
  "Remove the emulation mode map alist."
  (setq emulation-mode-map-alists
        (delq 'eme--emulation-mode-map-alist emulation-mode-map-alists)))

;;; Cursor Type Management

(defvar-local eme--saved-cursor-type nil
  "Saved cursor type before enabling modal editing.")

(defun eme--set-cursor-type (type)
  "Set cursor type to TYPE in current buffer."
  (setq-local cursor-type type))

(defvar eme--cursor-watcher-active nil
  "Non-nil when cursor type watcher is active to prevent recursion.")

(defun eme--cursor-type-watcher (_symbol newval operation where)
  "Watch cursor-type changes and enforce correct value for eme buffers.
Uses timer to correct cursor after the current set operation completes."
  (when (and (eq operation 'set)
             where
             (not eme--cursor-watcher-active)
             (buffer-local-value 'eme-local-mode where))
    (let ((expected (if (buffer-local-value 'eme-insert-mode where)
                        eme-insert-cursor-type
                      eme-selection-cursor-type)))
      (unless (eq newval expected)
        (run-with-timer 0 nil
                        (lambda (buf exp)
                          (when (buffer-live-p buf)
                            (let ((eme--cursor-watcher-active t))
                              (with-current-buffer buf
                                (setq cursor-type exp)))))
                        where expected)))))

;;; Insert Start Modes Detection

(defun eme--insert-start-mode-p ()
  "Return non-nil if current buffer's major-mode should start in Insert Mode.
Checks if major-mode is in `eme-insert-start-modes' or derived from one."
  (let ((modes eme-insert-start-modes)
        (found nil))
    (while (and modes (not found))
      (when (derived-mode-p (car modes))
        (setq found t))
      (setq modes (cdr modes)))
    found))

;;; Mode Transition Functions

(defun eme-enter-selection-mode ()
  "Enter Selection Mode."
  (interactive)
  (when eme-insert-mode
    (eme-insert-mode -1))
  (unless eme-selection-mode
    (eme-selection-mode 1)))

(defun eme-enter-insert-mode ()
  "Enter Insert Mode."
  (interactive)
  (when eme-selection-mode
    (eme-selection-mode -1))
  (unless eme-insert-mode
    (eme-insert-mode 1)))

(defun eme-exit-insert-mode ()
  "Exit Insert Mode and return to Selection Mode."
  (interactive)
  (eme-enter-selection-mode))

;;; Minor Mode Definitions

(define-minor-mode eme-selection-mode
  "Selection Mode for modal editing.
Movement creates selection. Actions operate on selection."
  :lighter ""
  :keymap eme-selection-mode-map
  (if eme-selection-mode
      (progn
        ;; Ensure Insert Mode is off
        (when eme-insert-mode
          (setq eme-insert-mode nil))
        ;; Set cursor
        (eme--set-cursor-type eme-selection-cursor-type)
        ;; Update indicator
        (eme--update-mode-indicator)
        ;; Run hook
        (run-hooks 'eme-selection-mode-enter-hook))
    ;; Exit
    (run-hooks 'eme-selection-mode-exit-hook)
    (eme--update-mode-indicator)))

(define-minor-mode eme-insert-mode
  "Insert Mode for modal editing.
Standard Emacs editing. Press C-g or ESC to return to Selection Mode."
  :lighter ""
  :keymap eme-insert-mode-map
  (if eme-insert-mode
      (progn
        ;; Ensure Selection Mode is off
        (when eme-selection-mode
          (setq eme-selection-mode nil))
        ;; Set cursor
        (eme--set-cursor-type eme-insert-cursor-type)
        ;; Update indicator
        (eme--update-mode-indicator)
        ;; Run hook
        (run-hooks 'eme-insert-mode-enter-hook))
    ;; Exit
    (run-hooks 'eme-insert-mode-exit-hook)
    (eme--update-mode-indicator)))

;;; Mode-line Indicator

(defface eme-selection-mode-indicator
  '((t :background "#4078f2" :foreground "white" :weight bold))
  "Face for Selection Mode indicator in mode-line."
  :group 'eme)

(defface eme-insert-mode-indicator
  '((t :background "#50a14f" :foreground "white" :weight bold))
  "Face for Insert Mode indicator in mode-line."
  :group 'eme)

(defvar-local eme--mode-indicator ""
  "Mode-line indicator showing current modal state.")

(defun eme--mode-line-indicator ()
  "Return mode-line indicator with appropriate face."
  (cond
   (eme-selection-mode
    (propertize " S " 'face 'eme-selection-mode-indicator))
   (eme-insert-mode
    (propertize " I " 'face 'eme-insert-mode-indicator))
   (t "")))

(defvar eme--mode-line-construct
  '(:eval (eme--mode-line-indicator))
  "Mode-line construct for eme indicator.")

(defun eme--update-mode-indicator ()
  "Update the mode-line indicator based on current modal state."
  (setq eme--mode-indicator
        (cond
         (eme-selection-mode "[S]")
         (eme-insert-mode "[I]")
         (t "")))
  (force-mode-line-update))

(defun eme--mode-line-construct-p (elt)
  "Return non-nil if ELT is our mode-line construct."
  (and (consp elt)
       (eq (car elt) :eval)
       (consp (cdr elt))
       (consp (cadr elt))
       (eq (car (cadr elt)) 'eme--mode-line-indicator)))

(defun eme--setup-mode-line ()
  "Add eme indicator to the beginning of mode-line."
  (let* ((fmt (default-value 'mode-line-format))
         (found nil))
    (dolist (elt fmt)
      (when (eme--mode-line-construct-p elt)
        (setq found t)))
    (unless found
      (setq-default mode-line-format
                    (cons '(:eval (eme--mode-line-indicator)) fmt)))))

(defun eme--teardown-mode-line ()
  "Remove eme indicator from mode-line."
  (let ((result nil))
    (dolist (elt (default-value 'mode-line-format))
      (unless (eme--mode-line-construct-p elt)
        (push elt result)))
    (setq-default mode-line-format (nreverse result))))

;;; Minibuffer Handling

(defun eme--minibuffer-setup ()
  "Setup function for minibuffer.
Ensures minibuffer uses Insert Mode."
  ;; In minibuffer, we don't use our modes at all
  ;; This allows normal Emacs editing
  nil)

(defun eme--minibuffer-exit ()
  "Exit function for minibuffer.
Restores previous buffer's modal state."
  nil)

;;; Local Mode

(define-minor-mode eme-local-mode
  "Buffer-local modal editing mode.
Enables modal editing in the current buffer only.
Modes in `eme-insert-start-modes' start in Insert Mode to preserve
their native keybindings."
  :lighter nil
  :keymap nil
  (if eme-local-mode
      (progn
        ;; Save cursor type
        (setq eme--saved-cursor-type cursor-type)
        ;; Setup emulation mode map
        (eme--setup-emulation-mode-map-alist)
        ;; Start in Insert Mode for insert-start-modes, otherwise Selection Mode
        (if (eme--insert-start-mode-p)
            (eme-enter-insert-mode)
          (eme-enter-selection-mode)))
    ;; Disable
    (eme-selection-mode -1)
    (eme-insert-mode -1)
    ;; Restore cursor
    (when eme--saved-cursor-type
      (setq cursor-type eme--saved-cursor-type))
    (eme--update-mode-indicator)))

;;; Global Mode

(defun eme--turn-on-modal-editing ()
  "Turn on modal editing in current buffer if appropriate.
All buffers get modal editing enabled, but modes in `eme-insert-start-modes'
start in Insert Mode to preserve their native keybindings."
  (unless (minibufferp)
    (eme-local-mode 1)))

;;;###autoload
(define-globalized-minor-mode eme-mode
  eme-local-mode
  eme--turn-on-modal-editing
  :group 'eme
  (if eme-mode
      (progn
        (eme--setup-emulation-mode-map-alist)
        (eme--setup-mode-line)
        (add-variable-watcher 'cursor-type #'eme--cursor-type-watcher)
        (add-hook 'minibuffer-setup-hook #'eme--minibuffer-setup)
        (add-hook 'minibuffer-exit-hook #'eme--minibuffer-exit))
    (eme--teardown-emulation-mode-map-alist)
    (eme--teardown-mode-line)
    (remove-variable-watcher 'cursor-type #'eme--cursor-type-watcher)
    (remove-hook 'minibuffer-setup-hook #'eme--minibuffer-setup)
    (remove-hook 'minibuffer-exit-hook #'eme--minibuffer-exit)))

;;; Goal Column for Line Movement

(defvar-local eme--goal-column nil
  "Goal column for line movement.
Preserved across line movements and actions on selection.")

(defvar-local eme--goal-column-valid nil
  "Non-nil if `eme--goal-column' should be used for next line movement.
Set to t by n/p, cleared by horizontal movements and undo.")

;;; Anchor State

(defvar-local eme--anchor-active nil
  "Non-nil if anchor was explicitly set with `.' (eme-toggle-anchor).
When t, movements extend selection from anchor.
When nil, movements create fresh selection.")

(defvar-local eme--anchor-bounds nil
  "Selection bounds when anchor was set.
A cons cell (START . END) representing the selection range at anchor time.
Used to extend eme-selection by merging with new movement range.")

;;; Delimiter Context

(defvar-local eme--delimiter-context nil
  "Context from last delimiter inner selection.
When non-nil, a cons cell (CHAR . BOUNDS) where:
- CHAR is the delimiter character
- BOUNDS is (OPEN-POS . CLOSE-POS) of the delimiter pair
Used for 2-stroke delimiter operations:
- Same delimiter key: delete delimiter
- Different delimiter key: change delimiter
- Action key: normal action on selection")

;;; Independent Selection Mechanism (eme-selection)

;; eme-selection provides selection independent of Emacs mark/point.
;; This allows cursor position and selection range to be managed separately.
;; Used for line movement where we want "cursor at column X" AND "whole line selected".

(defface eme-selection-face
  '((t :inherit region))
  "Face for eme-selection highlight."
  :group 'eme)

(defvar-local eme--selection-overlay nil
  "Overlay for eme-selection highlight.")

(defun eme-selection-active-p ()
  "Return t if eme-selection is active, nil otherwise."
  (and eme--selection-overlay
       (overlay-buffer eme--selection-overlay)))

(defun eme-selection-bounds ()
  "Return selection bounds as (START . END), or nil if no selection."
  (when (eme-selection-active-p)
    (cons (overlay-start eme--selection-overlay)
          (overlay-end eme--selection-overlay))))

(defun eme-selection-set (start end)
  "Set eme-selection from START to END.
Creates or updates the overlay to highlight the selection."
  (if (eme-selection-active-p)
      ;; Update existing overlay
      (move-overlay eme--selection-overlay start end)
    ;; Create new overlay
    (setq eme--selection-overlay (make-overlay start end nil nil t))
    (overlay-put eme--selection-overlay 'face 'eme-selection-face)
    (overlay-put eme--selection-overlay 'eme-selection t)
    ;; Evaporate when text is deleted - prevents undo from restoring
    (overlay-put eme--selection-overlay 'evaporate t)))

(defun eme-selection-clear ()
  "Clear eme-selection."
  (when eme--selection-overlay
    (delete-overlay eme--selection-overlay)
    (setq eme--selection-overlay nil)))

(provide 'eme-core)

;;; eme-core.el ends here
