;;; minimal-simple.el --- Common commands for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020-2025  Protesilaos Stavrou

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
;; Common commands for my Emacs: <https://protesilaos.com/emacs/dotemacs/>.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'minimal-common)

(defgroup minimal-simple ()
  "Generic utilities for my dotemacs."
  :group 'editing)

(defcustom minimal-simple-date-specifier "%F"
  "Date specifier for `format-time-string'.
Used by `minimal-simple-inset-date'."
  :type 'string
  :group 'minimal-simple)

(defcustom minimal-simple-time-specifier "%R %z"
  "Time specifier for `format-time-string'.
Used by `minimal-simple-inset-date'."
  :type 'string
  :group 'minimal-simple)

;;; Commands

;;;; General commands

;;;###autoload
(defun minimal-simple-describe-at-point (symbol)
  "Describe the SYMBOL at point.
If there is no symbol or the symbol at point does not satisfy `symbolp',
prompt for one."
  (interactive (list (intern-soft (thing-at-point 'symbol))))
  (if symbol
      (describe-symbol symbol)
    (call-interactively 'describe-symbol)))

;;;###autoload
(defun minimal-simple-indent-dwim ()
  "Indent the current defun in `prog-mode' or paragraph in `text-mode'."
  (interactive)
  (save-excursion
    (cond
     ((derived-mode-p 'prog-mode)
      (mark-defun))
     ((derived-mode-p 'text-mode)
      (mark-paragraph)))
    (indent-for-tab-command)
    (deactivate-mark)))

;;;###autoload
(defun minimal-simple-sudo ()
  "Find the current file or directory using `sudo'."
  (interactive)
  (let ((destination (or buffer-file-name default-directory))
        (auto-save-default nil))
    (if (string= (file-remote-p destination 'method) "sudo")
        (user-error "Already using `sudo'")
      (find-file (format "/sudo::/%s" destination)))))

(defun minimal-simple--mark (bounds)
  "Mark between BOUNDS as a cons cell of beginning and end positions."
  (push-mark (car bounds))
  (goto-char (cdr bounds))
  (activate-mark))

;;;###autoload
(defun minimal-simple-mark-sexp ()
  "Mark symbolic expression at or near point.
Repeat to extend the region forward to the next symbolic
expression."
  (interactive)
  (if (and (region-active-p)
           (eq last-command this-command))
      (ignore-errors (forward-sexp 1))
    (when-let* ((thing (cond
                        ((thing-at-point 'url) 'url)
                        ((thing-at-point 'sexp) 'sexp)
                        ((thing-at-point 'string) 'string)
                        ((thing-at-point 'word) 'word))))
      (minimal-simple--mark (bounds-of-thing-at-point thing)))))

;;;###autoload
(defun minimal-simple-keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.

- When a minibuffer is open, but not focused, close the minibuffer.  For
  recursive minibuffers, make sure to only close one level of depth.

- When in a *Completions* or `special-mode' buffer (e.g. *Help* or
  *Messages*), close it.

- When the *Completions* or a `special-mode' buffer is on display but
  not selected, close it.  If there are more than one, close them all.

- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((and (derived-mode-p 'completion-list-mode 'special-mode)
         (not (one-window-p)))
    (quit-window))
   ((when-let* ((_ (not (one-window-p)))
                (windows (seq-filter
                          (lambda (window)
                            (with-selected-window window
                              (derived-mode-p 'completion-list-mode 'special-mode)))
                          (window-list))))
      (dolist (window windows)
        (quit-window nil window))))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

;;;###autoload
(defun minimal-simple-delete-window-dwim ()
  "Do What I Mean to delete the current THING.
When there is more than one window, THING is a window.
When there are more than one `tab-bar-mode' tabs, THING is a tab.
Else THING is a frame if frames are more than one."
  (declare (interactive-only t))
  (interactive)
  (cond
   ((length> (window-list) 1)
    (delete-window))
   ((and (featurep 'tab-bar)
         (length> (tab-bar-tabs) 1))
    (tab-close))
   ((length> (frame-list) 1)
    (delete-frame))
   (t
    (user-error "Nothing to delete"))))

;; DEPRECATED 2023-12-26: The `minimal-simple-goto-definition' is a good
;; idea but it needs more work.  Ultimately though, it is easier to
;; just produce a Help buffer and just go to the source from there by
;; typing 's'.

;; (declare-function help--symbol-completion-table "help-fns" (string pred action))
;;
;; ;;;###autoload
;; (defun minimal-simple-goto-definition (symbol)
;;   "Prompt for SYMBOL and go to its source.
;; When called from Lisp, SYMBOL is a string."
;;   (interactive
;;    (list
;;     (completing-read "Go to source of SYMBOL: "
;;                      #'help--symbol-completion-table
;;                      nil :require-match)))
;;   (xref-find-definitions symbol))

;; DEPRECATED 2023-12-26: I have no need for these commands.  I was
;; just experimenting with a simple implementation.  It is not robust.
;; I can fix it, but I will still not use it, so I am deprecating it
;; instead.

;; (autoload 'number-at-point "thingatpt")
;;
;; (defun minimal-simple--number-operate (number amount operation)
;;   "Perform OPERATION on NUMBER given AMOUNT and return the result.
;; OPERATION is the keyword `:increment' or `:decrement' to perform
;; `1+' or `1-', respectively."
;;   (when (and (numberp number) (numberp amount))
;;     (let ((fn (pcase operation
;;                 (:increment #'+)
;;                 (:decrement #'-)
;;                 (_ (user-error "Unknown operation `%s' for number `%s'" operation number)))))
;;       (funcall fn number amount))))
;;
;; (defun minimal-simple--number-replace (number amount operation)
;;   "Perform OPERATION on NUMBER at point by AMOUNT."
;;   (when-let* ((bounds (bounds-of-thing-at-point 'number))
;;               (replacement (minimal-simple--number-operate number amount operation)))
;;     (delete-region (car bounds) (cdr bounds))
;;     (save-excursion
;;       (insert (number-to-string replacement)))))
;;
;; ;;;###autoload
;; (defun minimal-simple-number-increment (number amount)
;;   "Increment NUMBER by AMOUNT.
;; When called interactively, NUMBER is the one at point, while
;; AMOUNT is either 1 or that of a number prefix argument."
;;   (interactive
;;    (list
;;     (number-at-point)
;;     (prefix-numeric-value current-prefix-arg)))
;;   (minimal-simple--number-replace number amount :increment))
;;
;; ;;;###autoload
;; (defun minimal-simple-number-decrement (number amount)
;;   "Decrement NUMBER by AMOUNT.
;; When called interactively, NUMBER is the one at point, while
;; AMOUNT is either 1 or that of a number prefix argument."
;;   (interactive
;;    (list
;;     (number-at-point)
;;     (prefix-numeric-value current-prefix-arg)))
;;   (minimal-simple--number-replace number amount :decrement))

;;;; Commands for lines

;;;###autoload
(defun minimal-simple-new-line-below (n)
  "Create N empty lines below the current one.
When called interactively without a prefix numeric argument, N is
1."
  (interactive "p")
  (goto-char (line-end-position))
  (dotimes (_ n) (insert "\n")))

;;;###autoload
(defun minimal-simple-new-line-above (n)
  "Create N empty lines above the current one.
When called interactively without a prefix numeric argument, N is
1."
  (interactive "p")
  (let ((point-min (point-min)))
    (if (or (bobp)
            (eq (point) point-min)
            (eq (line-number-at-pos point-min) 1))
        (progn
          (goto-char (line-beginning-position))
          (dotimes (_ n) (insert "\n"))
          (forward-line (- n)))
      (forward-line (- n))
      (minimal-simple-new-line-below n))))

;;;###autoload
(defun minimal-simple-copy-line ()
  "Copy the current line to the `kill-ring'."
  (interactive)
  (copy-region-as-kill (line-beginning-position) (line-end-position)))

(make-obsolete 'minimal-simple-copy-line-or-region 'minimal-simple-copy-line "2023-09-26")

;;;###autoload
(defun minimal-simple-kill-ring-save (&optional beg end)
  "Copy the current region or line.
When the region is active, use `kill-ring-save' between the BEG and END
positions.  Otherwise, copy the current line."
  ;; NOTE 2025-02-23: Using (interactive "r") returns an error before
  ;; running the body of this function if there is no mark.  This
  ;; happens when visiting a file.
  (interactive
   (when (region-active-p)
     (list
      (region-beginning)
      (region-end))))
  (if (and beg end)
      (copy-region-as-kill beg end)
    (minimal-simple-copy-line))
  (setq this-command 'kill-ring-save))

;;;###autoload
(defun minimal-simple-kill-region (&optional beg end)
  "Do `kill-region' when the region is active, else `kill-ring-save' symbol at point."
  (interactive
   (when (region-active-p)
     (list
      (region-beginning)
      (region-end))))
  (if (and beg end)
      (kill-region beg end)
    (minimal-simple-mark-sexp)
    (copy-region-as-kill (region-beginning) (region-end)))
  (setq this-command 'kill-ring-save))

(defun minimal-simple--duplicate-buffer-substring (boundaries)
  "Duplicate buffer substring between BOUNDARIES.
BOUNDARIES is a cons cell representing buffer positions."
  (unless (consp boundaries)
    (error "`%s' is not a cons cell" boundaries))
  (let ((beg (car boundaries))
        (end (cdr boundaries)))
    (goto-char end)
    (newline)
    (insert (buffer-substring-no-properties beg end))))

;;;###autoload
(defun minimal-simple-duplicate-line-or-region ()
  "Duplicate the current line or active region."
  (interactive)
  (unless mark-ring                  ; needed when entering a new buffer
    (push-mark (point) t nil))
  (minimal-simple--duplicate-buffer-substring
   (if (region-active-p)
       (cons (region-beginning) (region-end))
     (cons (line-beginning-position) (line-end-position))))
  (setq this-command 'yank))

;;;###autoload
(defun minimal-simple-yank-replace-line-or-region ()
  "Replace line or region with latest kill.
This command can then be followed by the standard
`yank-pop' (default is bound to \\[yank-pop])."
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (line-beginning-position) (line-end-position)))
  (yank)
  (setq this-command 'yank))

;;;###autoload
(defun minimal-simple-multi-line-below ()
  "Move half a screen below."
  (interactive)
  (forward-line (floor (window-height) 2))
  (setq this-command 'scroll-up-command))

;;;###autoload
(defun minimal-simple-multi-line-above ()
  "Move half a screen above."
  (interactive)
  (forward-line (- (floor (window-height) 2)))
  (setq this-command 'scroll-down-command))

;;;###autoload
(defun minimal-simple-kill-line-backward ()
  "Kill from point to the beginning of the line."
  (interactive)
  (kill-line 0)
  (setq this-command 'kill-line))

;;;###autoload
(defun minimal-simple-copy-line-forward (n)
  "Copy from point to the end of the Nth line.
Without numeric prefix argument N, operate on the current line."
  (interactive "p")
  (let ((point (point))
        (end (line-end-position n))
        (max (point-max)))
    (copy-region-as-kill
     point
     (if (> end max)
         max
       end)))
  (setq this-command 'kill-ring-save))

;;;###autoload
(defun minimal-simple-copy-line-backward (n)
  "Copy from point to the beginning of the Nth line.
Without numeric prefix argument N, operate on the current line."
  (interactive "p")
  (let ((point (point))
        (beg (line-beginning-position n))
        (min (point-min)))
    (copy-region-as-kill
     point
     (if (< beg min)
         min
       beg)))
  (setq this-command 'kill-ring-save))

;;;###autoload
(defun minimal-simple-delete-line ()
  "Delete (not kill) from point to the end of the line."
  (interactive)
  (let* ((point (point))
         (end (line-end-position))
         (end+ (+ end 1)))
    (cond
     ((> end+ (point-max)) (delete-region point end))
     ((= point end) (delete-region point end+))
     (t (delete-region point end))))
  (setq this-command 'delete-region))

;;;###autoload
(defun minimal-simple-delete-line-backward ()
  "Delete (not kill) from point to the beginning of the line."
  (interactive)
  (let* ((point (point))
         (beg (line-beginning-position))
         (beg- (- beg 1)))
    (cond
     ((< beg- (point-min)) (delete-region beg point))
     ((= point beg) (delete-region beg- point))
     (t (delete-region beg point))))
  (setq this-command 'delete-region))

;;;###autoload
(define-minor-mode minimal-simple-auto-fill-visual-line-mode
  "Enable `visual-line-mode' and disable `auto-fill-mode' in the current buffer."
  :global nil
  (if minimal-simple-auto-fill-visual-line-mode
      (progn
        (auto-fill-mode -1)
        (visual-line-mode 1))
    (auto-fill-mode 1)
    (visual-line-mode -1)))

;;;; Commands for text insertion or manipulation

;;;###autoload
(defun minimal-simple-insert-date (&optional arg)
  "Insert the current date as `minimal-simple-date-specifier'.

With optional prefix ARG (\\[universal-argument]) also append the
current time understood as `minimal-simple-time-specifier'.

When region is active, delete the highlighted text and replace it
with the specified date."
  (interactive "P")
  (let* ((date minimal-simple-date-specifier)
         (time minimal-simple-time-specifier)
         (format (if arg (format "%s %s" date time) date)))
    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))
    (insert (format-time-string format))))

(defun minimal-simple--pos-url-on-line (char)
  "Return position of `minimal-common-url-regexp' at CHAR."
  (when (integer-or-marker-p char)
    (save-excursion
      (goto-char char)
      (re-search-forward minimal-common-url-regexp (line-end-position) :noerror))))

;;;###autoload
(defun minimal-simple-escape-url-line (char)
  "Escape all URLs or email addresses on the current line.
When called from Lisp CHAR is a buffer position to operate from
until the end of the line.  In interactive use, CHAR corresponds
to `line-beginning-position'."
  (interactive
   (list
    (if current-prefix-arg
        (re-search-forward
         minimal-common-url-regexp
         (line-end-position) :no-error
         (prefix-numeric-value current-prefix-arg))
      (line-beginning-position))))
  (when-let* ((regexp-end (minimal-simple--pos-url-on-line char)))
    (goto-char regexp-end)
    (unless (looking-at ">")
      (insert ">")
      (when (search-backward "\s" (line-beginning-position) :noerror)
        (forward-char 1))
      (insert "<"))
    (minimal-simple-escape-url-line (1+ regexp-end)))
  (goto-char (line-end-position)))

;; Thanks to Bruno Boal for the original `minimal-simple-escape-url-region'.
;; Check Bruno's Emacs config: <https://github.com/BBoal/emacs-config>.

;;;###autoload
(defun minimal-simple-escape-url-region (&optional beg end)
  "Apply `minimal-simple-escape-url-line' on region lines between BEG and END."
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (error "There is no region!")))
  (let ((beg (min beg end))
        (end (max beg end)))
    (save-excursion
      (goto-char beg)
      (setq beg (line-beginning-position))
      (while (<= beg end)
        (minimal-simple-escape-url-line beg)
        (beginning-of-line 2)
        (setq beg (point))))))

;;;###autoload
(defun minimal-simple-escape-url-dwim ()
  "Escape URL on the current line or lines implied by the active region.
Call the commands `minimal-simple-escape-url-line' and
`minimal-simple-escape-url-region' ."
  (interactive)
  (if (region-active-p)
      (minimal-simple-escape-url-region (region-beginning) (region-end))
    (minimal-simple-escape-url-line (line-beginning-position))))

;;;###autoload
(defun minimal-simple-zap-to-char-backward (char &optional arg)
  "Backward `zap-to-char' for CHAR.
Optional ARG is a numeric prefix to match ARGth occurance of
CHAR."
  (interactive
   (list
    (read-char-from-minibuffer "Zap to char: " nil 'read-char-history)
    (prefix-numeric-value current-prefix-arg)))
  (zap-to-char (- arg) char t))

(defvar minimal-simple-flush-and-diff-history nil
  "Minibuffer history for `minimal-simple-flush-and-diff'.")

;;;###autoload
(defun minimal-simple-flush-and-diff (regexp beg end)
  "Call `flush-lines' for REGEXP and produce diff if file is modified.
When region is active, operate between the region boundaries
demarcated by BEG and END."
  (interactive
   (let ((regionp (region-active-p)))
     (list
      (read-regexp "Flush lines using REGEXP: " nil 'minimal-simple-flush-and-diff-history)
      (and regionp (region-beginning))
      (and regionp (region-end)))))
  (flush-lines regexp (or beg (point-min)) (or end (point-max)) :no-message)
  (when (and (buffer-modified-p) buffer-file-name)
    (diff-buffer-with-file (current-buffer))))

;; FIXME 2023-09-28: The line prefix is problematic.  I plan to rewrite it.

;; (defcustom minimal-simple-line-prefix-strings '(">" "+" "-")
;;   "List of strings used as line prefixes.
;; The command which serves as the point of entry is
;; `minimal-simple-insert-line-prefix'."
;;   :type '(repeat string)
;;   :group 'minimal-simple)
;;
;; (defun minimal-simple--line-prefix-regexp (&optional string)
;;   "Format regular expression for `minimal-simple--line-prefix-p'.
;; With optional STRING use it directly.  Else format the regexp by
;; concatenating `minimal-simple-line-prefix-strings'."
;;   (if string
;;       (format "^%s " string)
;;     (format "^[%s] " (apply #'concat minimal-simple-line-prefix-strings))))
;;
;; (defun minimal-simple--line-prefix-p (&optional string)
;;   "Return non-nil if line beginning has an appropriate string prefix.
;; With optional STRING test that it is at the beginning of the line."
;;   (save-excursion
;;     (goto-char (line-beginning-position))
;;     (looking-at (minimal-simple--line-prefix-regexp string))))
;;
;; (defun minimal-simple--line-prefix-insert (string)
;;   "Insert STRING at the beginning of the line, followed by a space."
;;   (save-excursion
;;     (goto-char (line-beginning-position))
;;     (insert string)
;;     (insert " ")))
;;
;; (defun minimal-simple--line-prefix-infer-string ()
;;   "Return line prefix string if it matches `minimal-simple--line-prefix-p'."
;;   (when (minimal-simple--line-prefix-p)
;;     (string-trim
;;      (buffer-substring-no-properties (match-beginning 0) (match-end 0)))))
;;
;; (defun minimal-simple--line-prefix-toggle (string)
;;   "Insert or remove STRING at the beginning of the line."
;;   (if (minimal-simple--line-prefix-p string)
;;       (delete-region (match-beginning 0) (match-end 0))
;;     (minimal-simple--line-prefix-insert string)))
;;
;; (defvar minimal-simple--line-prefix-history nil
;;   "Minibuffer history of `minimal-simple--line-prefix-prompt'.")
;;
;; (defun minimal-simple--line-prefix-prompt ()
;;   "Prompt for string to use as line prefix.
;; Provide `minimal-simple-line-prefix-strings' as completion
;; candidates, though accept arbitrary input."
;;   (let ((default (car minimal-simple--line-prefix-history)))
;;     (completing-read
;;      (format-prompt "Select line prefix" default)
;;      minimal-simple-line-prefix-strings
;;      nil nil nil
;;      'minimal-simple--line-prefix-history default)))
;;
;; (defun minimal-simple-line-prefix-infer-or-prompt ()
;;   "Infer string for line prefix or prompt for one."
;;   (or (minimal-simple--line-prefix-infer-string)
;;       (minimal-simple--line-prefix-prompt)))
;;
;; ;;;###autoload
;; (defun minimal-simple-insert-line-prefix-dwim (string)
;;   "Toggle presence of STRING at the beginning of the line.
;;
;; When called interactively try to infer STRING based on the line
;; prefix.  If one is found among `minimal-simple-line-prefix-strings',
;; perform a removal outright.
;;
;; If no string can be inferred, prompt for STRING among
;; `minimal-simple-line-prefix-strings'.  Accept arbitrary strings at
;; the prompt.
;;
;; When the region is active, toggle the presence of STRING for each
;; line in the region."
;;   (interactive (list (minimal-simple-line-prefix-infer-or-prompt)))
;;   (if-let* ((region-p (region-active-p))
;;             (beg (region-beginning))
;;             (end (line-number-at-pos (region-end))))
;;       (progn
;;         (goto-char beg)
;;         (push-mark (point))
;;         (while (<= (line-number-at-pos (point)) end)
;;           (minimal-simple--line-prefix-toggle string)
;;           (forward-line 1)))
;;     (minimal-simple--line-prefix-toggle string)))

;;;; Commands for object transposition

;; The "move" functions all the way to `minimal-simple-move-below-dwim'
;; are courtesy of Bruno Boal: <https://git.sr.ht/~bboal>.  With minor
;; tweaks by me.
(defun minimal-simple--move-line (count dir)
  "Move line or region COUNTth times in DIR direction."
  (let* ((start (pos-bol))
         (end (pos-eol))
         diff-eol-point
         diff-eol-mark)
    (when-let* (((use-region-p))
                (pos (point))
                (mrk (mark))
                (line-diff-mark-point (1+ (- (line-number-at-pos mrk)
                                             (line-number-at-pos pos)))))
      (if (> pos mrk)
          (setq start (pos-bol line-diff-mark-point)) ; pos-bol of where the mark is
        (setq end (pos-eol line-diff-mark-point)))    ; pos-eol of the line where the mark is
      (setq diff-eol-mark (1+ (- end mrk))))          ; 1+ to get the \n
    ;; this is valid for region or a single line
    (setq diff-eol-point (1+ (- end (point))))
    (let* ((max (point-max))
           (end (1+ end))
           (end (if (> end max) max end))
           (deactivate-mark)
           (lines (delete-and-extract-region start end)))
      (forward-line (* count dir))
      ;; Handle the special case when there isn't a newline as the eob.
      (when (and (eq (point) max)
                 (/= (current-column) 0))
        (insert "\n"))
      (insert lines)
      ;; if user provided a region
      (when diff-eol-mark
        (set-mark (- (point) diff-eol-mark)))
      ;; either way go to same point location reference initial motion
      (goto-char (- (point) diff-eol-point)))))

(defun minimal-simple--move-line-user-error (boundary)
  "Return `user-error' with message accounting for BOUNDARY.
BOUNDARY is a buffer position, expected to be `point-min' or `point-max'."
  (when-let* ((bound (line-number-at-pos boundary))
              (scope (cond
                      ((and (use-region-p)
                            (or (= (line-number-at-pos (point)) bound)
                                (= (line-number-at-pos (mark)) bound)))
                       "region is ")
                      ((= (line-number-at-pos (point)) bound)
                       "")
                      (t nil))))
    (user-error (format "Warning: %salready in the last line!" scope))))

(defun minimal-simple-move-above-dwim (arg)
  "Move line or region ARGth times up.
If ARG is nil, do it one time."
  (interactive "p")
  (unless (minimal-simple--move-line-user-error (point-min))
    (minimal-simple--move-line arg -1)))

(defun minimal-simple-move-below-dwim (arg)
  "Move line or region ARGth times down.
If ARG is nil, do it one time."
  (interactive "p")
  (unless (minimal-simple--move-line-user-error (point-max))
    (minimal-simple--move-line arg 1)))

(defmacro minimal-simple-define-transpose (scope)
  "Define transposition command for SCOPE.
SCOPE is the text object to operate on.  The command's name is
minimal-simple-transpose-SCOPE."
  `(defun ,(intern (format "minimal-simple-transpose-%s" scope)) (arg)
     ,(format "Transpose %s.
Transposition over an active region will swap the object at
the region beginning with the one at the region end." scope)
     (interactive "p")
     (let ((fn (intern (format "%s-%s" "transpose" ,scope))))
       (if (use-region-p)
           (funcall fn 0)
         (funcall fn arg)))))

;;;###autoload (autoload 'minimal-simple-transpose-lines "minimal-simple")
;;;###autoload (autoload 'minimal-simple-transpose-paragraphs "minimal-simple")
;;;###autoload (autoload 'minimal-simple-transpose-sentences "minimal-simple")
;;;###autoload (autoload 'minimal-simple-transpose-sexps "minimal-simple")
;;;###autoload (autoload 'minimal-simple-transpose-words "minimal-simple")
(minimal-simple-define-transpose "lines")
(minimal-simple-define-transpose "paragraphs")
(minimal-simple-define-transpose "sentences")
(minimal-simple-define-transpose "sexps")
(minimal-simple-define-transpose "words")

;;;###autoload
(defun minimal-simple-transpose-chars ()
  "Always transposes the two characters before point.
There is no dragging the character forward.  This is the
behaviour of `transpose-chars' when point is at the end of the
line."
  (interactive)
  (if (eq (point) (line-end-position))
      (transpose-chars 1)
    (transpose-chars -1)
    (forward-char)))

;;;; Commands for paragraphs

;;;###autoload
(defun minimal-simple-unfill-region-or-paragraph ()
  "Unfill current paragraph or the active region."
  (interactive)
  (unless mark-ring ; needed when entering a new buffer
    (push-mark (point) t nil))
  (let ((fill-column most-positive-fixnum))
    (if (region-active-p)
        (fill-region (region-beginning) (region-end))
      (fill-paragraph))))

;;;; Commands for windows and pages

;;;###autoload
(defun minimal-simple-other-window ()
  "Wrapper for `other-window' and `next-multiframe-window'.
If there is only one window and multiple frames, call
`next-multiframe-window'.  Otherwise, call `other-window'."
  (interactive)
  (if (and (one-window-p) (length> (frame-list) 1))
      (progn
        (call-interactively #'next-multiframe-window)
        (setq this-command #'next-multiframe-window))
    (call-interactively #'other-window)
    (setq this-command #'other-window)))

;;;###autoload
(defun minimal-simple-narrow-visible-window ()
  "Narrow buffer to wisible window area.
Also check `minimal-simple-narrow-dwim'."
  (interactive)
  (let* ((bounds (minimal-common-window-bounds))
         (window-area (- (cdr bounds) (car bounds)))
         (buffer-area (- (point-max) (point-min))))
    (if (/= buffer-area window-area)
        (narrow-to-region (car bounds) (cdr bounds))
      (user-error "Buffer fits in the window; won't narrow"))))

;;;###autoload
(defun minimal-simple-narrow-dwim ()
  "Do-what-I-mean narrowing.
If region is active, narrow the buffer to the region's
boundaries.

If pages are defined by virtue of `minimal-common-page-p', narrow to
the current page boundaries.

If no region is active and no pages exist, narrow to the visible
portion of the window.

If narrowing is in effect, widen the view."
  (interactive)
  (unless mark-ring                  ; needed when entering a new buffer
    (push-mark (point) t nil))
  (cond
   ((and (use-region-p)
         (null (buffer-narrowed-p)))
    (narrow-to-region (region-beginning) (region-end)))
   ((minimal-common-page-p)
    (narrow-to-page))
   ((null (buffer-narrowed-p))
    (minimal-simple-narrow-visible-window))
   ((widen))))

(defun minimal-simple--narrow-to-page (count &optional back)
  "Narrow to COUNTth page with optional BACK motion."
  (if back
      (narrow-to-page (or (- count) -1))
    (narrow-to-page (or (abs count) 1)))
  ;; Avoids the problem of skipping pages while cycling back and forth.
  (goto-char (point-min)))

;;;###autoload
(defun minimal-simple-forward-page-dwim (&optional count)
  "Move to next or COUNTth page forward.
If buffer is narrowed to the page, keep the effect while
performing the motion.  Always move point to the beginning of the
narrowed page."
  (interactive "p")
  (if (buffer-narrowed-p)
      (minimal-simple--narrow-to-page count)
    (forward-page count)
    (setq this-command 'forward-page)))

;;;###autoload
(defun minimal-simple-backward-page-dwim (&optional count)
  "Move to previous or COUNTth page backward.
If buffer is narrowed to the page, keep the effect while
performing the motion.  Always move point to the beginning of the
narrowed page."
  (interactive "p")
  (if (buffer-narrowed-p)
      (minimal-simple--narrow-to-page count t)
    (backward-page count)
    (setq this-command 'backward-page)))

;;;###autoload
(defun minimal-simple-delete-page-delimiters (&optional beg end)
  "Delete lines with just page delimiters in the current buffer.
When region is active, only operate on the region between BEG and
END, representing the point and mark."
  (interactive "r")
  (let (b e)
    (if (use-region-p)
        (setq b beg
              e end)
      (setq b (point-min)
            e (point-max)))
    (widen)
    (flush-lines (format "%s$" page-delimiter) b e)
    (setq this-command 'flush-lines)))

;; NOTE 2023-06-18: The idea of narrowing to a defun in an indirect
;; buffer is still experimental.
(defun minimal-simple-narrow--guess-defun-symbol ()
  "Try to return symbol of current defun as a string."
  (save-excursion
    (beginning-of-defun)
    (search-forward " ")
    (thing-at-point 'symbol :no-properties)))

;;;###autoload
(defun minimal-simple-narrow-to-cloned-buffer ()
  "Narrow to defun in cloned buffer.
Name the buffer after the defun's symbol."
  (interactive)
  (clone-indirect-buffer-other-window
   (format "%s -- %s"
           (buffer-name)
           (minimal-simple-narrow--guess-defun-symbol))
   :display)
  (narrow-to-defun))

;;;; Commands for buffers

(defun minimal-simple--display-unsaved-buffers (buffers buffer-menu-name)
  "Produce buffer menu listing BUFFERS called BUFFER-MENU-NAME."
  (let ((old-buf (current-buffer))
        (buf (get-buffer-create buffer-menu-name)))
    (with-current-buffer buf
      (Buffer-menu-mode)
      (setq-local Buffer-menu-files-only nil
                  Buffer-menu-buffer-list buffers
                  Buffer-menu-filter-predicate nil)
      (list-buffers--refresh buffers old-buf)
      (tabulated-list-print))
    (display-buffer buf)))

(defun minimal-simple--get-unsaved-buffers ()
  "Get list of unsaved buffers."
  (seq-filter
   (lambda (buffer)
     (and (buffer-file-name buffer)
          (buffer-modified-p buffer)))
   (buffer-list)))

;;;###autoload
(defun minimal-simple-display-unsaved-buffers ()
  "Produce buffer menu listing unsaved file-visiting buffers."
  (interactive)
  (if-let* ((unsaved-buffers (minimal-simple--get-unsaved-buffers)))
      (minimal-simple--display-unsaved-buffers unsaved-buffers "*Unsaved buffers*")
    (message "No unsaved buffers")))

(defun minimal-simple-display-unsaved-buffers-on-exit (&rest _)
  "Produce buffer menu listing unsaved file-visiting buffers.
Add this as :before advice to `save-buffers-kill-emacs'."
  (when-let* ((unsaved-buffers (minimal-simple--get-unsaved-buffers)))
    (minimal-simple--display-unsaved-buffers unsaved-buffers "*Unsaved buffers*")))

;;;###autoload
(defun minimal-simple-copy-current-buffer-name ()
  "Add the current buffer's name to the `kill-ring'."
  (declare (interactive-only t))
  (interactive)
  (kill-new (buffer-name (current-buffer))))

;;;###autoload
(defun minimal-simple-copy-current-buffer-file ()
  "Add the current buffer's file path to the `kill-ring'."
  (declare (interactive-only t))
  (interactive)
  (if buffer-file-name
      (kill-new buffer-file-name)
    (user-error "%s is not associated with a file" (buffer-name (current-buffer)))))

;;;###autoload
(defun minimal-simple-kill-buffer (buffer)
  "Kill current BUFFER without confirmation.
When called interactively, prompt for BUFFER."
  (interactive (list (read-buffer "Select buffer: ")))
  (let ((kill-buffer-query-functions nil))
    (kill-buffer (or buffer (current-buffer)))))

(make-obsolete
 'minimal-simple-kill-buffer-current
 'minimal-simple-kill-buffer-dwim
 "2025-11-04")

;;;###autoload
(defun minimal-simple-kill-buffer-dwim (&optional arg)
  "Kill current buffer.
With optional prefix ARG (\\[universal-argument]) delete the
buffer's window as well.  Kill the window regardless of ARG if it
satisfies `minimal-common-window-small-p' and it has no previous
buffers in its history."
  (interactive "P")
  (let ((kill-buffer-query-functions nil))
    (cond
     ;; Is a tab whose last window's buffer is to be deleted.
     ((and (one-window-p)
           (length> (tab-bar-tabs) 1))
      (let ((buffer (current-buffer)))
        (tab-close)
        (kill-buffer buffer)))
     ;; Is an ancillary window that appeared for this buffer but is
     ;; otherwise not supposed to be there.
     ((and (minimal-common-window-small-p)
           (null (window-prev-buffers)))
      (kill-buffer-and-window))
     (t
      (kill-buffer)))))

;;;###autoload
(defun minimal-simple-rename-file-and-buffer (name)
  "Apply NAME to current file and rename its buffer.
Do not try to make a new directory or anything fancy."
  (interactive
   (list (read-string "Rename current file: " (buffer-file-name))))
  (let ((file (buffer-file-name)))
    (if (vc-registered file)
        (vc-rename-file file name)
      (rename-file file name))
    (set-visited-file-name name t t)))

(defun minimal-simple--buffer-major-mode-prompt ()
  "Prompt of `minimal-simple-buffers-major-mode'.
Limit list of buffers to those matching the current
`major-mode' or its derivatives."
  (let ((read-buffer-function nil)
        (current-major-mode major-mode))
    (read-buffer
     (format "Buffer for %s: " major-mode)
     nil
     :require-match
     (lambda (pair) ; pair is (name-string . buffer-object)
       (with-current-buffer (cdr pair)
         (derived-mode-p current-major-mode))))))

;;;###autoload
(defun minimal-simple-buffers-major-mode ()
  "Select BUFFER matching the current one's major mode."
  (interactive)
  (switch-to-buffer (minimal-simple--buffer-major-mode-prompt)))

(defun minimal-simple--buffer-vc-root-prompt ()
  "Prompt of `minimal-simple-buffers-vc-root'."
  (let ((root (or (vc-root-dir)
                  (locate-dominating-file "." ".git")))
        (read-buffer-function nil))
    (read-buffer
     (format "Buffers in %s: " root)
     nil t
     (lambda (pair) ; pair is (name-string . buffer-object)
       (with-current-buffer (cdr pair) (string-match-p root default-directory))))))

;;;###autoload
(defun minimal-simple-buffers-vc-root ()
  "Select buffer matching the current one's VC root."
  (interactive)
  (switch-to-buffer (minimal-simple--buffer-vc-root-prompt)))

;;;###autoload
(defun minimal-simple-swap-window-buffers (counter)
  "Swap states of live buffers.
With two windows, transpose their buffers.  With more windows,
perform a clockwise rotation.  Do not alter the window layout.
Just move the buffers around.

With COUNTER as a prefix argument, do the rotation
counter-clockwise."
  (interactive "P")
  (when-let* ((winlist (if counter (reverse (window-list)) (window-list)))
              (wincount (count-windows))
              ((> wincount 1)))
    (dotimes (i (- wincount 1))
      (window-swap-states (elt winlist i) (elt winlist (+ i 1))))))

;;;; Commands of a general nature

(autoload 'color-rgb-to-hex "color")
(autoload 'color-name-to-rgb "color")

(defun minimal-simple-accessible-colors (variant)
  "Return list of accessible `defined-colors'.
VARIANT is either `dark' or `light'."
  (let ((variant-color (if (eq variant 'black) "#000000" "#ffffff")))
    (seq-filter
     (lambda (c)
       (let* ((rgb (color-name-to-rgb c))
              (r (nth 0 rgb))
              (g (nth 1 rgb))
              (b (nth 2 rgb))
              (hex (color-rgb-to-hex r g b 2)))
         (when (>= (minimal-common-contrast variant-color hex) 4.5)
           c)))
     (defined-colors))))

(defun minimal-simple--list-accessible-colors-prompt ()
  "Use `read-multiple-choice' to return white or black background."
  (intern
   (cadr
    (read-multiple-choice
     "Variant"
     '((?b "black" "Black background")
       (?w "white" "White background"))
     "Choose between white or black background."))))

;;;###autoload
(defun minimal-simple-list-accessible-colors (variant)
  "Return buffer with list of accessible `defined-colors'.
VARIANT is either `dark' or `light'."
  (interactive (list (minimal-simple--list-accessible-colors-prompt)))
  (list-colors-display (minimal-simple-accessible-colors variant)))

(defun minimal-simple-update-package-repositories-pull (package package-directory buffer)
  "Pull PACKAGE which extends PACKAGE-DIRECTORY.
Use BUFFER for standard output and return the exit code."
  (let ((default-directory package-directory))
    (message "Pulling %s from directory %s" package default-directory)
    (call-process "git" nil (list buffer t) nil "pull")))

(defun minimal-simple-update-package-repositories-clone (package base-directory buffer)
  "Clone PACKAGE to an extension of BASE-DIRECTORY.
Use BUFFER for standard output and return the exit code."
  (message "Cloning %s to directory %s" package base-directory)
  (call-process "git" nil (list buffer t) nil "clone" (format "git@github.com:protesilaos/%s %s" package base-directory)))

(define-error 'minimal-package-no-update "Package could not be updated" 'error)

(defun minimal-simple-update-package-repositories-subr (packages)
  "Pull or clone all repositories of my PACKAGES."
  (unless (executable-find "git")
    (user-error "Cannot find git program; install it first or add it to the $PATH; aborting"))
  (unless (getenv "SSH_AUTH_SOCK")
    (user-error "Cannot find $SSH_AUTH_SOCK; check your SSH connection; aborting"))
  (let ((stdout (get-buffer-create " *minimal-simple-git-package-stdout*")))
    (dolist (package packages)
      (let* ((name (cond
                    ((symbolp package) (symbol-name package))
                    ((stringp package) package)
                    (t (error "The `%s' is neither a symbol nor a string" package))))
             (common-directory (expand-file-name "~/Git/Projects/"))
             (package-directory (expand-file-name name common-directory))
             (exit-code (if (file-directory-p package-directory)
                            (minimal-simple-update-package-repositories-pull package package-directory stdout)
                          (minimal-simple-update-package-repositories-clone package common-directory stdout))))
        (condition-case error-data
            (when (> exit-code 0)
              (signal 'minimal-package-no-update (list (format "Package `%s' got exit code `%s'" package exit-code))))
          (:success
           (message "Updated `%s' repository" package))
          (minimal-package-no-update
           ;; TODO 2025-11-06: Is it safe to stash changes outright?
           ;; I think it is fine, but maybe there is a case where this
           ;; can lead to data loss?
           (when (file-directory-p package-directory)
             (let ((default-directory package-directory))
               (unless (fboundp 'vc-git-stash)
                 (require 'vc-git))
               (vc-git-stash (format "minimal-package-update %s: %s" (format-time-string "%FT%T") package))))
           (message "Tried to stash changes in package `%s' because: %s" package (cdr error-data)))
          ((error user-error)
           (message "The package `%s' returned error data: %s" package error-data))
          (quit
           (message "Aborted by the user")))))))

(defvar minimal-simple-update-package-repositories-prompt-history nil
  "Minibuffer history of `minimal-simple-update-package-repositories-prompt'.")

(defun minimal-simple-update-package-repositories-prompt ()
  "Prompt for packages among `minimal-emacs-my-packages'."
  (let ((default (car minimal-simple-update-package-repositories-prompt-history)))
    (completing-read-multiple
     (format-prompt "Select packages" default)
     minimal-emacs-my-packages
     nil t nil
     'minimal-simple-update-package-repositories-prompt-history
     default)))

;;;###autoload
(defun minimal-simple-update-some-or-all-of-my-package-repositories (packages &optional all-packages)
  "Prompt for PACKAGES among `minimal-emacs-my-packages' to pull or clone.
With a universal prefix argument for ALL-PACKAGES, do not prompt for packages and
update them all instead."
  (interactive
   (list
    (if current-prefix-arg
        minimal-emacs-my-packages
      (minimal-simple-update-package-repositories-prompt))))
  (minimal-simple-update-package-repositories-subr packages))

;;;; Global minor mode to override key maps

(defvar minimal-simple-override-mode-map (make-sparse-keymap)
  "Key map of `minimal-simple-override-mode'.
Enable that mode to have its key bindings take effect over those of the
major mode.")

(define-minor-mode minimal-simple-override-mode
  "Enable the `minimal-simple-override-mode-map'."
  :init-value nil
  :global t
  :keymap minimal-simple-override-mode-map)

(provide 'minimal-simple)
;;; minimal-simple.el ends here
