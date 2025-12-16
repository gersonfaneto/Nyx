;;; minimal-notmuch.el --- Tweaks for my notmuch.el configurations -*- lexical-binding: t -*-

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
;; This covers my tweaks for notmuch.el that are meant for use in my
;; Emacs setup: https://protesilaos.com/emacs/dotemacs.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(require 'minimal-common)
(eval-when-compile (require 'cl-lib))

(defgroup minimal-notmuch ()
  "Extensions for notmuch.el."
  :group 'notmuch)

(defcustom minimal-notmuch-delete-tag "del"
  "Single tag that applies to mail marked for deletion.
This is used by `minimal-notmuch-delete-mail'."
  :type 'string
  :group 'minimal-notmuch)

(defcustom minimal-notmuch-mark-delete-tags
  `(,(format "+%s" minimal-notmuch-delete-tag) "-inbox" "-unread")
  "List of tags to mark for deletion.
To actually delete email, refer to `minimal-notmuch-delete-mail'."
  :type '(repeat string)
  :group 'minimal-notmuch)

(defcustom minimal-notmuch-mark-flag-tags '("+flag" "-unread")
  "List of tags to mark as important (flagged).
This gets the `notmuch-tag-flagged' face, if that is specified in
`notmuch-tag-formats'."
  :type '(repeat string)
  :group 'minimal-notmuch)

(defcustom minimal-notmuch-mark-spam-tags '("+spam" "-inbox" "-unread")
  "List of tags to mark as spam."
  :type '(repeat string)
  :group 'minimal-notmuch)

;;;; Commands

(autoload 'notmuch-interactive-region "notmuch")
(autoload 'notmuch-tag-change-list "notmuch")
(autoload 'notmuch-search-next-thread "notmuch")
(autoload 'notmuch-search-tag "notmuch")

(defmacro minimal-notmuch-search-tag-thread (name tags)
  "Produce NAME function parsing TAGS."
  (declare (indent defun))
  `(defun ,name (&optional untag beg end)
     ,(format
       "Mark with `%s' the currently selected thread.

Operate on each message in the currently selected thread.  With
optional BEG and END as points delimiting a region that
encompasses multiple threads, operate on all those messages
instead.

With optional prefix argument (\\[universal-argument]) as UNTAG,
reverse the application of the tags.

This function advances to the next thread when finished."
       tags)
     (interactive (cons current-prefix-arg (notmuch-interactive-region)))
     (when ,tags
       (notmuch-search-tag
        (notmuch-tag-change-list ,tags untag) beg end))
     (when (eq beg end)
       (notmuch-search-next-thread))))

(minimal-notmuch-search-tag-thread
  minimal-notmuch-search-delete-thread
  minimal-notmuch-mark-delete-tags)

(minimal-notmuch-search-tag-thread
  minimal-notmuch-search-flag-thread
  minimal-notmuch-mark-flag-tags)

(minimal-notmuch-search-tag-thread
  minimal-notmuch-search-spam-thread
  minimal-notmuch-mark-spam-tags)

(defmacro minimal-notmuch-show-tag-message (name tags)
  "Produce NAME function parsing TAGS."
  (declare (indent defun))
  `(defun ,name (&optional untag)
     ,(format
       "Apply `%s' to message.

With optional prefix argument (\\[universal-argument]) as UNTAG,
reverse the application of the tags."
       tags)
     (interactive "P")
     (when ,tags
       (apply 'notmuch-show-tag-message
	          (notmuch-tag-change-list ,tags untag)))))

(minimal-notmuch-show-tag-message
  minimal-notmuch-show-delete-message
  minimal-notmuch-mark-delete-tags)

(minimal-notmuch-show-tag-message
  minimal-notmuch-show-flag-message
  minimal-notmuch-mark-flag-tags)

(minimal-notmuch-show-tag-message
  minimal-notmuch-show-spam-message
  minimal-notmuch-mark-spam-tags)

(autoload 'notmuch-refresh-this-buffer "notmuch")
(autoload 'notmuch-refresh-all-buffers "notmuch")

(defun minimal-notmuch-refresh-buffer (&optional arg)
  "Run `notmuch-refresh-this-buffer'.
With optional prefix ARG (\\[universal-argument]) call
`notmuch-refresh-all-buffers'."
  (interactive "P")
  (if arg
      (notmuch-refresh-all-buffers)
    (notmuch-refresh-this-buffer)))

;;;###autoload
(defun minimal-notmuch-delete-mail ()
  "Permanently delete mail marked as `minimal-notmuch-delete-mail'.
Prompt for confirmation before carrying out the operation.

Do not attempt to refresh the index.  This will be done upon the
next invocation of 'notmuch new'."
  (interactive)
  (let* ((del-tag minimal-notmuch-delete-tag)
         (count
          (string-to-number
           (with-temp-buffer
             (shell-command
              (format "notmuch count tag:%s" minimal-notmuch-delete-tag) t)
             (buffer-substring-no-properties (point-min) (1- (point-max))))))
         (mail (if (> count 1) "mails" "mail")))
    (unless (> count 0)
      (user-error "No mail marked as `%s'" del-tag))
    (when (yes-or-no-p
           (format "Delete %d %s marked as `%s'?" count mail del-tag))
      (shell-command
       (format "notmuch search --output=files --format=text0 tag:%s | xargs -r0 rm" del-tag)
       t))))

;;;; SourceHut-related setup

(defconst minimal-notmuch-patch-control-codes
  '("PROPOSED" "NEEDS_REVISION" "SUPERSEDED" "APPROVED" "REJECTED" "APPLIED")
  "Control codes for SourceHut patches.
See `minimal-notmuch-patch-add-email-control-code' for how to apply
them.")

(defun minimal-notmuch--rx-in-sourcehut-mail (rx-group string)
  "Return RX-GROUP of SourceHut mail in STRING."
  (when (string-match-p "lists\\.sr\\.ht" string)
    (string-clean-whitespace
     (replace-regexp-in-string
      ".*?[<]?\\(\\([-a-zA-Z0-9=._+~/]+\\)@\\(lists\\.sr\\.ht\\)\\)[>]?.*?"
      (format "\\%s" rx-group) string))))

(declare-function notmuch-show-get-header "notmuch-show" (header &optional props))
(declare-function message-fetch-field "message" (header &optional first))

(defun minimal-notmuch--get-to-or-cc-header ()
  "Get appropriate To or Cc header."
  (cond
   ((derived-mode-p 'notmuch-message-mode)
    (concat (message-fetch-field "To") " " (message-fetch-field "Cc")))
   ((derived-mode-p 'notmuch-show-mode)
    (concat (notmuch-show-get-header :To) " " (notmuch-show-get-header :Cc)))))

;; NOTE 2022-04-19: This assumes that we only have one list...  I think
;; that is okay, but it might cause problems.
(defun minimal-notmuch--extract-sourcehut-mail (rx-group)
  "Extract RX-GROUP from SourceHut mailing list address.
1 is the full email address, 2 is the local part, while 3 is the
domain."
  (minimal-notmuch--rx-in-sourcehut-mail
   rx-group (minimal-notmuch--get-to-or-cc-header)))

(declare-function message-add-header "message" (&rest headers))

;; Read: <https://man.sr.ht/lists.sr.ht/#email-controls>.
;;;###autoload
(defun minimal-notmuch-patch-add-email-control-code (control-code)
  "Add custom header for SourceHut email controls.
The CONTROL-CODE is among `minimal-notmuch-patch-control-codes'."
  (interactive
   (list (completing-read "Select control code: " minimal-notmuch-patch-control-codes nil t)))
  (if (member control-code minimal-notmuch-patch-control-codes)
    (unless (message-fetch-field "X-Sourcehut-Patchset-Update")
      (message-add-header (format "X-Sourcehut-Patchset-Update: %s" control-code)))
    (user-error "%s is not specified in `minimal-notmuch-patch-control-codes'" control-code)))

;;;###autoload
(defun minimal-notmuch-ask-sourcehut-control-code ()
  "Use `minimal-notmuch-patch-add-email-control-code' programmatically.
Add this to `notmuch-mua-send-hook'."
  (when-let* ((header (message-fetch-field "Subject"))
              (subject (when (>= (length header) 6) (substring header 0 6)))
              ((string= "[PATCH" subject)) ; Is [ always there?
              ((minimal-notmuch--extract-sourcehut-mail 1))
              ((not (message-fetch-field "X-Sourcehut-Patchset-Update")))
              ((y-or-n-p "Add control code for SourceHut PATCH?")))
    (call-interactively #'minimal-notmuch-patch-add-email-control-code)))

;; NOTE 2022-04-19: Ideally we should be able to use the
;; `notmuch-show-stash-mlarchive-link-alist' for
;; `minimal-notmuch-stash-sourcehut-link', but it assumes that the base URL
;; is fixed for all message IDs, whereas those on SourceHut are not.

(declare-function notmuch-show-get-message-id "notmuch-show" (&optional bare))
(declare-function notmuch-show-message-top "notmuch-show")
(declare-function notmuch-common-do-stash "notmuch-lib" (text))

;;;###autoload
(defun minimal-notmuch-stash-sourcehut-link (&optional current)
  "Stash web link to current SourceHut thread.
With optional CURRENT argument, produce a link to the current
message, else use the topmost message (start of the thread).

Note that the topmost message is assumed to hold the id of the
base URL, though this is not necessarily true."
  (interactive "P")
  (let* ((ml (minimal-notmuch--extract-sourcehut-mail 2))
         (base-id (save-excursion (goto-char (point-min))
                                  (notmuch-show-message-top)
                                  (notmuch-show-get-message-id t)))
         (current-id (notmuch-show-get-message-id t)))
    (notmuch-common-do-stash
     (if current
         (format "https://lists.sr.ht/%s/<%s>#<%s>" ml base-id current-id)
       (format "https://lists.sr.ht/%s/<%s>" ml base-id)))))

;;;###autoload
(defun minimal-notmuch-check-valid-sourcehut-email ()
  "Check if SourceHut address is correct.
Add this to `notmuch-mua-send-hook'."
  (when-let* ((ml (minimal-notmuch--extract-sourcehut-mail 1))
              ((not (string-match-p "^\\(~\\|\\.\\)" ml)))
              ((not (y-or-n-p "SourceHut address looks wrong.  Send anyway?"))))
    (user-error "Incorrect SourceHut address")))

(provide 'minimal-notmuch)
;;; minimal-notmuch.el ends here
