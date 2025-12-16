;;; minimal-abbrev.el --- Functions for use with abbrev-mode -*- lexical-binding: t -*-

;; Copyright (C) 2025-2025  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Functions for use with `abbrev-mode'.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(defgroup minimal-abbrev ()
  "Functions for use with `abbrev-mode'."
  :group 'editing)

(defcustom minimal-abbrev-time-specifier "%R"
  "Time specifier for `format-time-string'."
  :type 'string
  :group 'minimal-abbrev)

(defcustom minimal-abbrev-date-specifier "%F"
  "Date specifier for `format-time-string'."
  :type 'string
  :group 'minimal-abbrev)

(defun minimal-abbrev-current-time ()
  "Insert the current time per `minimal-abbrev-time-specifier'."
  (insert (format-time-string minimal-abbrev-time-specifier)))

(defun minimal-abbrev-current-date ()
  "Insert the current date per `minimal-abbrev-date-specifier'."
  (insert (format-time-string minimal-abbrev-date-specifier)))

(defun minimal-abbrev-jitsi-link ()
  "Insert a Jitsi link."
  (insert (concat "https://meet.jit.si/" (format-time-string "%Y%m%dT%H%M%S"))))

(defvar minimal-abbrev-update-html-history nil
  "Minibuffer history for `minimal-abbrev-update-html-prompt'.")

(defun minimal-abbrev-update-html-prompt ()
  "Minibuffer prompt for `minimal-abbrev-update-html'.
Use completion among previous entries, retrieving their data from
`minimal-abbrev-update-html-history'."
  (completing-read
   "Insert update for manual: "
   minimal-abbrev-update-html-history
   nil nil nil 'minimal-abbrev-update-html-history))

(defun minimal-abbrev-update-html ()
  "Insert message to update NAME.html page, by prompting for NAME."
  (insert (format "Update %s.html" (minimal-abbrev-update-html-prompt))))

(defvar minimal-abbrev-org-macro-key-history nil
  "Minibuffer history for `minimal-abbrev-org-macro-key-prompt'.")

(defun minimal-abbrev-org-macro-key-prompt ()
  "Minibuffer prompt for `minimal-abbrev-org-macro-key'.
Use completion among previous entries, retrieving their data from
`minimal-abbrev-org-macro-key-history'."
  (completing-read
   "Key binding: "
   minimal-abbrev-org-macro-key-history
   nil nil nil 'minimal-abbrev-org-macro-key-history))

(defvar minimal-abbrev-org-macro-key-symbol-history nil
  "Minibuffer history for `minimal-abbrev-org-macro-key-symbol-prompt'.")

(defun minimal-abbrev-org-macro-key-symbol-prompt ()
  "Minibuffer prompt for `minimal-abbrev-org-macro-key'.
Use completion among previous entries, retrieving their data from
`minimal-abbrev-org-macro-key-symbol-history'."
  (completing-read
   "Command name: "
   minimal-abbrev-org-macro-key-symbol-history
   nil nil nil 'minimal-abbrev-org-macro-key-symbol-history))

(defun minimal-abbrev-org-macro-key-command ()
  "Insert {{{kbd(KEY)}}} (~SYMBOL~) by prompting for KEY and SYMBOL."
  (insert (format "{{{kbd(%s)}}} (~%s~)"
                  (minimal-abbrev-org-macro-key-prompt)
                  (minimal-abbrev-org-macro-key-symbol-prompt))))

(defun minimal-abbrev-org-macro-key ()
  "Insert {{{kbd(KEY)}}} by prompting for KEY."
  (insert (format "{{{kbd(%s)}}}" (minimal-abbrev-org-macro-key-prompt))))

(provide 'minimal-abbrev)
;;; minimal-abbrev.el ends here
