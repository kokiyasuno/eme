;;; eme-search.el --- Search integration for eme -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Koki Yasuno

;;; Commentary:

;; Search integration using isearch:
;; - /: start forward search
;; - M-/: start regexp search
;; - s: go to next match
;; - r: go to previous match
;;
;; Search ends with selection of the matched text.

;;; Code:

(require 'isearch)
(require 'eme-core)

;;; Variables

(defvar eme--search-last-string nil
  "Last search string used.")

(defvar eme--search-last-regexp-p nil
  "Non-nil if last search was regexp.")

;;; Isearch Integration

(defun eme--isearch-exit-with-selection ()
  "Exit isearch and select the matched text using eme-selection."
  (when (and isearch-success isearch-other-end)
    (let ((beg (min (point) isearch-other-end))
          (end (max (point) isearch-other-end)))
      (eme-selection-set beg end))))


(defun eme--isearch-end ()
  "End function for isearch integration."
  ;; Save last search string
  (setq eme--search-last-string isearch-string)
  (setq eme--search-last-regexp-p isearch-regexp)
  ;; Select match if successful
  (eme--isearch-exit-with-selection))

;;; Search Commands

(defun eme-search-forward ()
  "Start forward incremental search.
Clears eme-selection before starting search."
  (interactive)
  (eme-selection-clear)
  (add-hook 'isearch-mode-end-hook #'eme--isearch-end)
  (unwind-protect
      (isearch-forward)
    (remove-hook 'isearch-mode-end-hook #'eme--isearch-end)))

(defun eme-search-regexp ()
  "Start forward regexp incremental search."
  (interactive)
  (add-hook 'isearch-mode-end-hook #'eme--isearch-end)
  (unwind-protect
      (isearch-forward-regexp)
    (remove-hook 'isearch-mode-end-hook #'eme--isearch-end)))

;;; Navigation Commands

(defun eme-search-next ()
  "Go to next occurrence of last search string.
Select the matched text."
  (interactive)
  (if eme--search-last-string
      (let ((case-fold-search (isearch-no-upper-case-p
                               eme--search-last-string
                               eme--search-last-regexp-p))
            (search-fn (if eme--search-last-regexp-p
                           #'re-search-forward
                         #'search-forward))
            (start-pos (point))
            found)
        ;; Move past current position to find next
        (forward-char 1)
        (setq found (funcall search-fn eme--search-last-string nil t))
        (if found
            (progn
              ;; Select the match using eme-selection
              (eme-selection-set (match-beginning 0) (match-end 0))
              (goto-char (match-end 0)))
          ;; Wrap around
          (goto-char (point-min))
          (setq found (funcall search-fn eme--search-last-string nil t))
          (if found
              (progn
                (message "Search wrapped to beginning")
                (eme-selection-set (match-beginning 0) (match-end 0))
                (goto-char (match-end 0)))
            (goto-char start-pos)
            (message "No match found"))))
    (message "No previous search")))

(defun eme-search-previous ()
  "Go to previous occurrence of last search string.
Select the matched text."
  (interactive)
  (if eme--search-last-string
      (let ((case-fold-search (isearch-no-upper-case-p
                               eme--search-last-string
                               eme--search-last-regexp-p))
            (search-fn (if eme--search-last-regexp-p
                           #'re-search-backward
                         #'search-backward))
            (start-pos (point))
            found)
        (setq found (funcall search-fn eme--search-last-string nil t))
        (if found
            (progn
              ;; Select the match using eme-selection
              (eme-selection-set (match-beginning 0) (match-end 0))
              (goto-char (match-beginning 0)))
          ;; Wrap around
          (goto-char (point-max))
          (setq found (funcall search-fn eme--search-last-string nil t))
          (if found
              (progn
                (message "Search wrapped to end")
                (eme-selection-set (match-beginning 0) (match-end 0))
                (goto-char (match-beginning 0)))
            (goto-char start-pos)
            (message "No match found"))))
    (message "No previous search")))

(provide 'eme-search)

;;; eme-search.el ends here
