;;; eme-leader.el --- Leader Key system for eme -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Koki Yasuno

;;; Commentary:

;; Leader Key system provides unified access to Emacs prefix commands
;; from both Selection Mode and Insert Mode.
;;
;; Activation:
;; - SPC in Selection Mode (customizable via `eme-leader-key')
;; - M-SPC in Insert Mode (customizable via `eme-leader-key-insert')
;;
;; Default behavior (C-c prefix):
;; - Any key → C-c <key> (mode-specific-map)
;; - SPC <key> → C-c C-<key> (adds Control modifier)
;;
;; Prefix switching (customizable via `eme-leader-prefix-alist'):
;; - x → C-x prefix
;; - h → C-h prefix
;;
;; Special keys:
;; - u → universal-argument
;; - m → access eme-user-map

;;; Code:

(require 'eme-core)

;;; Customization Variables

(defcustom eme-leader-key (kbd "SPC")
  "Key to activate Leader Mode in Selection Mode."
  :type 'key-sequence
  :group 'eme)

(defcustom eme-leader-key-insert (kbd "M-SPC")
  "Key to activate Leader Mode in Insert Mode."
  :type 'key-sequence
  :group 'eme)

(defcustom eme-leader-prefix-alist
  '(("x" . "C-x")
    ("h" . "C-h"))
  "Alist mapping keys to prefix key sequences in Leader Mode.
Each entry is (KEY . PREFIX) where KEY is pressed after SPC
and PREFIX is the Emacs prefix to simulate."
  :type '(alist :key-type string :value-type string)
  :group 'eme)

;;; User Map

(defvar eme-user-map (make-sparse-keymap)
  "Keymap for user-defined bindings accessible via Leader Mode.
Access with SPC m (Selection Mode) or M-SPC m (Insert Mode).")

;;; State Variables

(defvar-local eme-leader--add-control nil
  "Non-nil to add C- modifier to next key.")

(defvar-local eme-leader--current-prefix nil
  "Current prefix key sequence (e.g., \"C-x\").")

;;; Core Functions

(defun eme-leader-start ()
  "Start Leader Mode."
  (interactive)
  ;; Set initial prefix
  (setq eme-leader--current-prefix "C-c")
  ;; Push C-c first, then read and process next key
  (setq unread-command-events
        (listify-key-sequence (kbd "C-c")))
  ;; Use pre-command-hook to intercept the next key
  (add-hook 'pre-command-hook #'eme-leader--intercept nil t))

(defun eme-leader--intercept ()
  "Intercept the next command after C-c prefix."
  (remove-hook 'pre-command-hook #'eme-leader--intercept t)
  (let* ((keys (this-command-keys))
         (last-key (if (> (length keys) 0)
                       (aref keys (1- (length keys)))
                     nil))
         (key-str (and last-key (single-key-description last-key)))
         (prefix-entry (assoc key-str eme-leader-prefix-alist)))
    (cond
     ;; SPC adds C- modifier to next key
     ((equal key-str "SPC")
      (setq this-command 'ignore)
      (setq eme-leader--add-control t)
      (add-hook 'pre-command-hook #'eme-leader--intercept-control nil t))
     ;; Redirect to another prefix
     (prefix-entry
      (setq this-command 'ignore)
      (setq eme-leader--add-control nil)
      (setq eme-leader--current-prefix (cdr prefix-entry))
      (setq unread-command-events
            (listify-key-sequence (kbd (cdr prefix-entry))))
      ;; Add intercept hook for the new prefix too
      (add-hook 'pre-command-hook #'eme-leader--intercept nil t))
     ;; Universal argument
     ((equal key-str "u")
      (setq eme-leader--add-control nil)
      (setq this-command 'universal-argument))
     ;; User map
     ((equal key-str "m")
      (setq this-command 'ignore)
      (setq eme-leader--add-control nil)
      (set-transient-map eme-user-map))
     ;; Normal key - reset add-control flag
     (t
      (setq eme-leader--add-control nil)))))

(defun eme-leader--intercept-control ()
  "Intercept next key and add C- modifier, prepending current prefix."
  (remove-hook 'pre-command-hook #'eme-leader--intercept-control t)
  (when eme-leader--add-control
    (setq eme-leader--add-control nil)
    (let* ((keys (this-command-keys))
           (last-key (if (> (length keys) 0)
                         (aref keys (1- (length keys)))
                       nil)))
      (when last-key
        (setq this-command 'ignore)
        ;; Create prefix + control-modified key
        (let ((ctrl-key (event-convert-list (list 'control last-key))))
          (setq unread-command-events
                (append (listify-key-sequence (kbd eme-leader--current-prefix))
                        (list ctrl-key))))))))


(provide 'eme-leader)

;;; eme-leader.el ends here
