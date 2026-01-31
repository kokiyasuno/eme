;;; eme.el --- Modal editing with Emacs keybindings -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Koki Yasuno

;; Author: Koki Yasuno
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, editing
;; URL: https://github.com/kyas/eme

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; eme provides modal editing for Emacs using familiar Emacs keybindings
;; (n/p/f/b) instead of Vim-style hjkl.
;;
;; The paradigm is "selection -> action" inspired by Kakoune and Helix,
;; where movement creates selection, then actions operate on that selection.
;;
;; Main features:
;; - Selection Mode: Movement creates selection using Emacs keys
;; - Insert Mode: Standard Emacs editing
;; - Thing selection: Delimiter-based selection with auto-detect
;; - Actions: Operations on selected region (delete, copy, yank, change)
;;
;; To enable globally:
;;   (eme-mode 1)

;;; Code:

(require 'eme-core)
(require 'eme-selection)
(require 'eme-delimiter)
(require 'eme-actions)
(require 'eme-search)
(require 'eme-goto-view)
(require 'eme-macro-register)
(require 'eme-thing)
(require 'eme-expand)
(require 'eme-leader)
(require 'eme-keybinds)

;;;###autoload
(defun eme-setup ()
  "Setup eme.
This function is called when the package is loaded."
  (interactive)
  (eme-mode 1))

(provide 'eme)

;;; eme.el ends here
