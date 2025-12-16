;;; minimal-comment.el --- Extensions newcomment.el for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2021-2025  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
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
;; This covers my newcomment.el extras, for use in my Emacs setup:
;; https://protesilaos.com/emacs/dotemacs.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(require 'newcomment)
(require 'minimal-common)

(defgroup minimal-comment ()
  "Extensions for newcomment.el."
  :group 'comment)

(defcustom minimal-comment-keywords
  '("TODO" "NOTE" "XXX" "REVIEW" "FIXME")
  "List of strings with keywords used by `minimal-comment-timestamp-keyword'."
  :type '(repeat string)
  :group 'minimal-comment)

(defcustom minimal-comment-timestamp-format-concise "%F"
  "Specifier for date in `minimal-comment-timestamp-keyword'.
Refer to the doc string of `format-time-string' for the available
options."
  :type 'string
  :group 'minimal-comment)

(defcustom minimal-comment-timestamp-format-verbose "%F %T %z"
  "Like `minimal-comment-timestamp-format-concise', but longer."
  :type 'string
  :group 'minimal-comment)

;;;###autoload
(defun minimal-comment (n)
  "Comment N lines, defaulting to the current one.
When the region is active, comment its lines instead."
  (interactive "p")
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-line n)))

(make-obsolete 'minimal-comment-comment-dwim 'minimal-comment "2023-09-28")

(defvar minimal-comment--keyword-hist '()
  "Minibuffer history of `minimal-comment--keyword-prompt'.")

(defun minimal-comment--keyword-prompt (keywords)
  "Prompt for candidate among KEYWORDS (per `minimal-comment-timestamp-keyword')."
  (let ((def (car minimal-comment--keyword-hist)))
    (completing-read
     (format "Select keyword [%s]: " def)
     keywords nil nil nil 'minimal-comment--keyword-hist def)))

(defun minimal-comment--format-date (verbose)
  "Format date using `format-time-string'.
VERBOSE has the same meaning as `minimal-comment-timestamp-keyword'."
  (format-time-string
   (if verbose
       minimal-comment-timestamp-format-verbose
     minimal-comment-timestamp-format-concise)))

(defun minimal-comment--timestamp (keyword &optional verbose)
  "Format string using current time and KEYWORD.
VERBOSE has the same meaning as `minimal-comment-timestamp-keyword'."
  (format "%s %s: " keyword (minimal-comment--format-date verbose)))

(defun minimal-comment--format-comment (string)
  "Format comment STRING per `minimal-comment-timestamp-keyword'.
STRING is a combination of a keyword and a time stamp."
  (concat comment-start
          (make-string comment-add (string-to-char comment-start))
          comment-padding
          string
          comment-end))

(defun minimal-comment--maybe-newline ()
  "Call `newline' if current line is not empty.
Check `minimal-comment-timestamp-keyword' for the rationale."
  (unless (minimal-common-line-regexp-p 'empty 1)
    (save-excursion (newline))))

;;;###autoload
(defun minimal-comment-timestamp-keyword (keyword &optional verbose)
  "Add timestamped comment with KEYWORD.

When called interactively, the list of possible keywords is that
of `minimal-comment-keywords', though it is possible to input
arbitrary text.

If point is at the beginning of the line or if line is empty (no
characters at all or just indentation), the comment is started
there in accordance with `comment-style'.  Any existing text
after the point will be pushed to a new line and will not be
turned into a comment.

If point is anywhere else on the line and the line is not empty,
the comment is appended to the line with `comment-indent'.

The comment is always formatted as DELIMITER KEYWORD DATE:, with
the date format being controlled by the variable
`minimal-comment-timestamp-format-concise'.  DELIMITER is the value
of `comment-start', as defined by the current major mode.

With optional VERBOSE argument (such as a prefix argument), use
an alternative date format, as specified by
`minimal-comment-timestamp-format-verbose'."
  (interactive
   (list
    (minimal-comment--keyword-prompt minimal-comment-keywords)
    current-prefix-arg))
  (let ((string (minimal-comment--timestamp keyword verbose))
        (beg (point)))
    (cond
     ((minimal-common-line-regexp-p 'empty)
      (insert (minimal-comment--format-comment string)))
     ((eq beg (line-beginning-position))
      (insert (minimal-comment--format-comment string))
      (indent-region beg (point))
      (minimal-comment--maybe-newline))
     (t
      (comment-indent t)
      (insert (concat " " string))))))

(provide 'minimal-comment)
;;; minimal-comment.el ends here
