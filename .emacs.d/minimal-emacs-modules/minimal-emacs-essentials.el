;;; Essential configurations
(minimal-emacs-configure

;;;; General settings and common custom functions (minimal-simple.el)
  (setq blink-matching-paren nil)
  (setq custom-unlispify-tag-names nil)
  (setq delete-pair-blink-delay 0.1) ; Emacs28 -- see `minimal-simple-delete-pair-dwim'
  (setq delete-pair-push-mark t) ; Emacs 31
  (setq echo-keystrokes-help nil) ; Emacs 30
  (setq epa-keys-select-method 'minibuffer) ; Emacs 30
  (setq eval-expression-print-length nil)
  (setq find-library-include-other-files nil) ; Emacs 29
  (setq help-window-select t)
  (setq help-window-keep-selected t) ; Emacs 29
  (setq kill-do-not-save-duplicates t)
  (setq mode-require-final-newline 'visit-save)
  (setq next-error-recenter '(4)) ; center of the window
  (setq remote-file-name-inhibit-auto-save t)                 ; Emacs 30
  (setq remote-file-name-inhibit-delete-by-moving-to-trash t) ; Emacs 30
  (setq save-interprogram-paste-before-kill t)
  (setq scroll-error-top-bottom t)
  (setq tramp-connection-timeout (* 60 10)) ; seconds
  (setq trusted-content '("~/Developer/Personal/")) ; Emacs 30
  (setq truncate-partial-width-windows nil)

  ;; Keys I unbind here are either to avoid accidents or to bind them

  (minimal-emacs-keybind global-map
    "<f2>" #'toggle-input-method  ; F2 overrides that two-column gimmick.  Sorry, but no!
    "<insert>" #'nil
    "<menu>" #'nil
    "C-x C-d" #'nil ; never use it
    "C-x C-v" #'nil ; never use it
    "C-z" #'nil ; I have a window manager, thanks!
    "C-x C-z" #'nil ; same idea as above
    "C-x C-c" #'nil ; avoid accidentally exiting Emacs
    "C-x C-c C-c" #'save-buffers-kill-emacs ; more cumbersome, less error-prone
    "C-h h" #'nil ; Never show that "hello" file
    "M-`" #'nil
    "M-o" #'delete-blank-lines ; alias for C-x C-o
    "M-SPC" #'cycle-spacing
    "M-z" #'zap-up-to-char ; NOT `zap-to-char'
    "M-c" #'capitalize-dwim
    "M-l" #'downcase-dwim ; "lower" case
    "M-u" #'upcase-dwim
    "M-=" #'count-words
    "C-x O" #'next-multiframe-window
    "C-h K" #'describe-keymap ; overrides `Info-goto-emacs-key-command-node'
    "C-h u" #'apropos-user-option
    "C-h F" #'apropos-function ; lower case is `describe-function'
    "C-h V" #'apropos-variable ; lower case is `describe-variable'
    "C-h L" #'apropos-library ; lower case is `view-lossage'
    "C-h c" #'describe-char) ; overrides `describe-key-briefly'

  (minimal-emacs-keybind prog-mode-map
    "C-M-d" #'up-list ; confusing name for what looks like "down" to me
    "<C-M-backspace>" #'backward-kill-sexp)

  ;; Keymap for buffers (Emacs28)
  (minimal-emacs-keybind ctl-x-x-map
    "f" #'follow-mode  ; override `font-lock-update'
    "r" #'rename-uniquely
    "l" #'visual-line-mode))

(minimal-emacs-configure
  (require 'minimal-common)

  (defvar prot/fundamental-mode-hook nil
    "Normal hook for `fundamental-mode' (which is missing by default).")

  (defun prot/fundamental-mode-run-hook (&rest args)
    "Apply ARGS and then run `prot/fundamental-mode-hook'."
    (apply args)
    (run-hooks 'prot/fundamental-mode-hook))

  (advice-add #'fundamental-mode :around #'prot/fundamental-mode-run-hook)

  (minimal-emacs-hook
    (text-mode-hook prog-mode-hook dired-mode-hook prot/fundamental-mode-hook hexl-mode-hook comint-mode-hook)
    minimal-common-truncate-lines-silently)

  ;; NEVER tell me which key can call a command that I specifically
  ;; invoked with M-x: I have a good reason to use it that way.
  (advice-add #'execute-extended-command--describe-binding-msg :override #'minimal-common-ignore))

(minimal-emacs-configure
  (require 'minimal-simple)

  (setq minimal-simple-date-specifier "%F")
  (setq minimal-simple-time-specifier "%R %z")

  (advice-add #'save-buffers-kill-emacs :before #'minimal-simple-display-unsaved-buffers-on-exit)

  ;; All `minimal-simple-override-mode' does is activate a key map.
  ;; Below I add keys to that map.  Because the mode is enabled
  ;; globally, those keys take precedence over the ones specified by
  ;; any given major mode.  In principle, this means that my keys will
  ;; always work (though technically they can be overriden by another
  ;; minor mode, depending on which one is evaluated last).
  (minimal-simple-override-mode 1)

  (minimal-emacs-keybind minimal-simple-override-mode-map
    "C-a" #'minimal-simple-duplicate-line-or-region ; "again" mnemonic, overrides `move-beginning-of-line'
    "C-d" #'minimal-simple-delete-line      ; overrides `delete-char'
    "C-v" #'minimal-simple-multi-line-below ; overrides `scroll-up-command'
    "<next>" #'minimal-simple-multi-line-below ; overrides `scroll-up-command'
    "M-v" #'minimal-simple-multi-line-above ; overrides `scroll-down-command'
    "<prior>" #'minimal-simple-multi-line-above ; overrides `scroll-down-command'
    "C-M-i" #'minimal-simple-indent-dwim ; overrides `completion-at-point'
    "C-M-\\" #'minimal-simple-indent-dwim ; overrides `indent-region'
    "C-M-c" #'completion-at-point) ; overrides `exit-recursive-edit'

  (minimal-emacs-keybind global-map
    "c-h h" #'minimal-simple-describe-at-point
    "<escape>" #'minimal-simple-keyboard-quit-dwim
    "C-g" #'minimal-simple-keyboard-quit-dwim
    "C-M-SPC" #'minimal-simple-mark-sexp
    ;; Commands for lines
    "C-S-d" #'minimal-simple-delete-line-backward
    "C-S-k" #'minimal-simple-kill-line-backward
    "M-k" #'minimal-simple-copy-line-forward
    "M-K" #'minimal-simple-copy-line-backward
    "M-j" #'delete-indentation
    "C-w" #'minimal-simple-kill-region
    "M-w" #'minimal-simple-kill-ring-save
    "C-S-w" #'minimal-simple-copy-line
    "C-S-y" #'minimal-simple-yank-replace-line-or-region
    "<C-return>" #'minimal-simple-new-line-below
    "<C-S-return>" #'minimal-simple-new-line-above
    "C-x x a" #'minimal-simple-auto-fill-visual-line-mode ; auto-fill/visual-line toggle
    ;; Commands for text insertion or manipulation
    "C-=" #'minimal-simple-insert-date
    "C-<" #'minimal-simple-escape-url-dwim
    "C->" #'minimal-simple-escape-url-dwim
    "M-Z" #'minimal-simple-zap-to-char-backward
    ;; Commands for object transposition
    "C-S-p" #'minimal-simple-move-above-dwim
    "C-S-n" #'minimal-simple-move-below-dwim
    "C-t" #'minimal-simple-transpose-chars
    "C-x C-t" #'minimal-simple-transpose-lines
    "C-S-t" #'minimal-simple-transpose-paragraphs
    "C-x M-t" #'minimal-simple-transpose-sentences
    "C-M-t" #'minimal-simple-transpose-sexps
    "M-t" #'minimal-simple-transpose-words
    ;; Commands for paragraphs
    "M-Q" #'minimal-simple-unfill-region-or-paragraph
    ;; Commands for windows and pages
    "C-x o" #'minimal-simple-other-window
    "C-x n k" #'minimal-simple-delete-page-delimiters
    "M-r" #'window-layout-transpose ; Emacs 31 override `move-to-window-line-top-bottom'
    "M-S-r" #'rotate-windows-back ; Emacs 31
    ;; Commands for buffers
    "<C-f2>" #'minimal-simple-rename-file-and-buffer
    "C-x k" #'minimal-simple-kill-buffer-dwim
    "C-x K" #'kill-buffer ; leaving this here to contrast with the above
    "M-s b" #'minimal-simple-buffers-major-mode
    "M-s v" #'minimal-simple-buffers-vc-root))

;;;; Scratch buffers per major mode (minimal-scratch.el)
(minimal-emacs-configure
  (setq minimal-scratch-default-mode 'text-mode)
  (autoload #'minimal-scratch-buffer "minimal-scratch")
  (define-key global-map (kbd "C-c s") #'minimal-scratch-buffer))

;;;; Insert character pairs (minimal-pair.el)
(minimal-emacs-configure
  (minimal-emacs-autoload
    (minimal-pair-insert minimal-pair-insert-directly minimal-pair-delete)
    "minimal-pair")
  (minimal-emacs-keybind global-map
    "C-'" #'minimal-pair-insert
    "M-'" #'minimal-pair-insert-directly
    "M-\\" #'minimal-pair-delete))

;;;; Comments (minimal-comment.el)
(minimal-emacs-configure
  (setq comment-empty-lines t)
  (setq comment-fill-column nil)
  (setq comment-multi-line t)
  (setq comment-style 'multi-line)
  (setq-default comment-column 0)

  (setq minimal-comment-comment-keywords '("TODO" "NOTE" "FIXME"))
  (setq minimal-comment-timestamp-format-concise "%F")
  (setq minimal-comment-timestamp-format-verbose "%F %T %z")

  (minimal-emacs-autoload
    (minimal-comment minimal-comment-timestamp-keyword)
    "minimal-comment")

  (minimal-emacs-keybind global-map
    "C-;" #'minimal-comment
    "M-;" #'minimal-comment ; overrides `comment-dwim'
    "C-x C-;" #'minimal-comment-timestamp-keyword))

;;;; Prefix keymap (minimal-prefix.el)
(minimal-emacs-configure
  (require 'minimal-prefix)
  (minimal-emacs-keybind global-map
    "<insert>" #'minimal-prefix
    "C-z" #'minimal-prefix))

(minimal-emacs-configure
  (define-key global-map (kbd "C-x C-r") #'recentf-open) ; override `find-file-read-only'

  (with-eval-after-load 'recentf
    (setq recentf-max-saved-items 100)
    (setq recentf-max-menu-items 25) ; I don't use the `menu-bar-mode', but this is good to know
    (setq recentf-save-file-modes nil)
    (setq recentf-keep nil)
    (setq recentf-auto-cleanup nil)
    (setq recentf-initialize-file-name-history nil)
    (setq recentf-filename-handlers nil)
    (setq recentf-show-file-shortcuts-flag nil)
    (recentf-mode 1)))

;;;; Mouse and mouse wheel behaviour
(minimal-emacs-configure
  (mouse-wheel-mode 1)
  ;; Some of these variables are defined in places other than
  ;; mouse.el, but this is fine.
  (setq mouse-autoselect-window t) ; complements the auto-selection of my tiling window manager
  (setq focus-follows-mouse t)

  ;; In Emacs 27+, use Control + mouse wheel to scale text.
  (setq mouse-wheel-scroll-amount
        '(1
          ((shift) . 5)
          ((meta) . 0.5)
          ((control) . text-scale))
        mouse-drag-copy-region nil
        make-pointer-invisible t
        mouse-wheel-progressive-speed t
        mouse-wheel-follow-mouse t)

  ;; Scrolling behaviour
  (setq scroll-preserve-screen-position t
        scroll-conservatively 1 ; affects `scroll-step'
        scroll-margin 0
        next-screen-context-lines 0))

;;;; Repeatable key chords (repeat-mode)
(minimal-emacs-configure
  (repeat-mode 1)

  (setq repeat-on-final-keystroke t
        repeat-exit-timeout 5
        repeat-exit-key "<escape>"
        repeat-keep-prefix nil
        repeat-check-key t
        repeat-echo-function 'ignore
        ;; Technically, this is not in repeal.el, though it is the
        ;; same idea.
        set-mark-command-repeat-pop t))

;;;; Built-in bookmarking framework (bookmark.el)
(minimal-emacs-configure
  (add-hook 'bookmark-bmenu-mode-hook #'hl-line-mode)
  (setq bookmark-use-annotations nil)
  (setq bookmark-automatically-show-annotations nil)
  (setq bookmark-fringe-mark nil) ; Emacs 29 to hide bookmark fringe icon
  ;; Write changes to the bookmark file as soon as 1 modification is
  ;; made (addition or deletion).  Otherwise Emacs will only save the
  ;; bookmarks when it closes, which may never happen properly
  ;; (e.g. power failure).
  (setq bookmark-save-flag 1))

;;;; Registers (register.el) and my extensions (minimal-register.el)
(minimal-emacs-configure
  (minimal-emacs-autoload
    (minimal-simple-file-to-register
     minimal-register-add-dwim
     minimal-register-use-dwim)
    "minimal-register")

  (minimal-emacs-keybind global-map
    "C-, ," #'minimal-register-add-dwim
    "C-, ." #'minimal-register-use-dwim
    "C-, /" #'bookmark-jump) ; alternattive to C-x r b

  (minimal-emacs-hook
    minimal-simple-file-to-register-jump-hook
    (pulsar-recenter-center pulsar-reveal-entry)
    nil
    pulsar)

  (with-eval-after-load 'register
    (setq register-preview-delay 0.8
          register-preview-function #'register-preview-default))

  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'register-alist)))

;;;; Auto revert mode
(minimal-emacs-configure
  (global-auto-revert-mode 1)
  (setq auto-revert-verbose t))

;;;; Delete selection
(minimal-emacs-configure
  (delete-selection-mode 1))

;;;; Tooltips (tooltip-mode)
(minimal-emacs-configure
  (tooltip-mode 1)
  (setq tooltip-delay 0.5
        tooltip-short-delay 0.5
        x-gtk-use-system-tooltips t
        tooltip-frame-parameters
        '((name . "tooltip")
          (internal-border-width . 10)
          (border-width . 0)
          (no-special-glyphs . t))))

;;;; Display current time
(minimal-emacs-configure
  (setq display-time-format " %a %e/%m, %H:%M ")
  ;;;; Covered by `display-time-format'
  ;; (setq display-time-24hr-format t)
  ;; (setq display-time-day-and-date t)
  (setq display-time-interval 60)
  (setq display-time-default-load-average nil)
  ;; NOTE 2022-09-21: For all those, I have implemented my own solution
  ;; that also shows the number of new items, although it depends on
  ;; notmuch: the `notmuch-indicator' package.
  (setq display-time-mail-directory nil)
  (setq display-time-mail-function nil)
  (setq display-time-use-mail-icon nil)
  (setq display-time-mail-string nil)
  (setq display-time-mail-face nil)

  ;; I don't need the load average and the mail indicator, so let this
  ;; be simple:
  (setq display-time-string-forms
        '((propertize
           (format-time-string display-time-format now)
           'face 'display-time-date-and-time
           'help-echo (format-time-string "%a %b %e, %Y" now))
          " "))

  (display-time-mode 1))

;;;; World clock (M-x world-clock)
(minimal-emacs-configure
  (setq display-time-world-list t)
  (setq zoneinfo-style-world-list ; M-x shell RET timedatectl list-timezones
        '(("America/Los_Angeles" "Los Angeles")
          ("America/Vancouver" "Vancouver")
          ("America/Chicago" "Chicago")
          ("America/Toronto" "Toronto")
          ("America/New_York" "New York")
          ("UTC" "UTC")
          ("Europe/Lisbon" "Lisbon")
          ("Europe/Brussels" "Brussels")
          ("Europe/Athens" "Athens")
          ("Asia/Riyadh" "Riyadh")
          ("Asia/Tbilisi" "Tbilisi")
          ("Asia/Singapore" "Singapore")
          ("Asia/Shanghai" "Shanghai")
          ("Asia/Seoul" "Seoul")
          ("Asia/Tokyo" "Tokyo")
          ("Australia/Brisbane" "Brisbane")
          ("Australia/Sydney" "Sydney")
          ("Pacific/Auckland" "Auckland")))

  ;; All of the following variables are for Emacs 28
  (setq world-clock-list t)
  (setq world-clock-time-format "%z %R	%a %d %b (%Z)")
  (setq world-clock-buffer-name "*world-clock*") ; Placement handled by `display-buffer-alist'
  (setq world-clock-timer-enable t)
  (setq world-clock-timer-second 60))

;;;; `man' (manpages)
(minimal-emacs-configure
  (setq Man-notify-method 'pushy)) ; does not obey `display-buffer-alist'

;;;; `proced' (process monitor, similar to `top')
(minimal-emacs-configure
  (setq proced-auto-update-flag 'visible) ; Emacs 30 supports more the `visible' value
  (setq proced-enable-color-flag t) ; Emacs 29
  (setq proced-auto-update-interval 5)
  (setq proced-descend t)
  (setq proced-filter 'user))

;;;; Emacs server (allow emacsclient to connect to running session)
(minimal-emacs-configure
  (setq server-client-instructions nil)
  (require 'server)
  (unless (or (server-running-p) (daemonp))
    (server-start)))

;;; Substitute
;; Another package of mine... Video demo:
;; <https://protesilaos.com/codelog/2023-01-16-emacs-substitute-package-demo/>.
(minimal-emacs-configure
  (minimal-emacs-install substitute)
  (require 'substitute)

  ;; Produce a message after the substitution that reports on what
  ;; happened.  It is a single line, like "Substituted `TARGET' with
  ;; `SUBSTITUTE' N times across the buffer.
  (add-hook 'substitute-post-replace-hook #'substitute-report-operation)

  ;; Set this to non-nil to highlight all occurrences of the current
  ;; target.
  (setopt substitute-highlight t)

  ;; Set this to t if you want to always treat the letter casing
  ;; literally.  Otherwise each command accepts a `C-u' prefix
  ;; argument to do this on-demand.
  (setq substitute-fixed-letter-case nil)

  ;; C-c s is occupied by `minimal-scratch-buffer'.
  (define-key global-map (kbd "C-c r") #'substitute-prefix-map))

(minimal-emacs-configure
  (minimal-emacs-install goto-chg)
  (minimal-emacs-keybind global-map
    "C-(" #'goto-last-change
    "C-)" #'goto-last-change-reverse))

;;; TMR May Ring (tmr is used to set timers)
;; Read the manual: <https://protesilaos.com/emacs/tmr>.
(minimal-emacs-configure
  (minimal-emacs-install tmr)
  (define-key global-map (kbd "C-c t") #'tmr-prefix-map)
  (setq tmr-sound-file "/usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga"
        tmr-notification-urgency 'normal
        tmr-description-list 'tmr-description-history))

;;; Pass interface (password-store)
(minimal-emacs-configure
  (minimal-emacs-install password-store)

  ;; Mnemonic is the root of the "code" word (κώδικας).  But also to add
  ;; the password to the kill-ring.  Other options are already taken.
  (define-key global-map (kbd "C-c k") #'password-store-copy)

  (setq password-store-time-before-clipboard-restore 30)

  (minimal-emacs-install pass))

;;; Generic interface for shells or REPLs (comint)
(minimal-emacs-configure
  ;; Support for OS-specific escape sequences such as what `ls
  ;; --hyperlink' uses.  I normally don't use those, but I am checking
  ;; this to see if there are any obvious advantages/disadvantages.
  (add-hook 'comint-output-filter-functions #'comint-osc-process-output)

  (setq ansi-color-for-comint-mode t) ; also see `ansi-color-for-compilation-mode'
  (setq comint-prompt-read-only t)
  (setq comint-buffer-maximum-size 9999)
  (setq comint-completion-autolist t)
  (setq comint-input-ignoredups t)
  (setq-default comint-scroll-to-bottom-on-input t)
  (setq-default comint-scroll-to-bottom-on-output nil)
  (setq-default comint-input-autoexpand 'input))

;;; Compilation interface (M-x compile)
(minimal-emacs-configure
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
  (setq ansi-color-for-compilation-mode t)) ; also see `ansi-color-for-comint-mode'

;;; Standard Unix Shell (M-x shell)
(minimal-emacs-configure
  (with-eval-after-load 'shell
    (minimal-emacs-keybind shell-mode-map
      "C-c C-k" #'comint-clear-buffer
      "C-c C-w" #'comint-write-output)

    ;; Check my .bashrc which handles `comint-terminfo-terminal':
    ;;
    ;; # Default pager.  The check for the terminal is useful for Emacs with
    ;; # M-x shell (which is how I usually interact with bash these days).
    ;; #
    ;; # The COLORTERM is documented in (info "(emacs) General Variables").
    ;; # I found the reference to `dumb-emacs-ansi' in (info "(emacs)
    ;; # Connection Variables").
    ;; if [ "$TERM" = "dumb" ] && [ "$INSIDE_EMACS" ] || [ "$TERM" = "dumb-emacs-ansi" ] && [ "$INSIDE_EMACS" ]
    ;; then
    ;;     export PAGER="cat"
    ;;     alias less="cat"
    ;;     export TERM=dumb-emacs-ansi
    ;;     export COLORTERM=1
    ;; else
    ;;     # Quit once you try to scroll past the end of the file.
    ;;     export PAGER="less --quit-at-eof"
    ;; fi
    (setq shell-command-prompt-show-cwd t) ; Emacs 27.1
    (setq shell-input-autoexpand 'input)
    (setq shell-highlight-undef-enable t) ; Emacs 29.1
    (setq shell-has-auto-cd nil) ; Emacs 29.1
    (setq shell-get-old-input-include-continuation-lines t) ; Emacs 30.1
    (setq shell-kill-buffer-on-exit t) ; Emacs 29.1
    (setq shell-completion-fignore '("~" "#" "%"))
    (setq tramp-default-remote-shell "/bin/bash")

    (setq shell-font-lock-keywords
          '(("[ \t]\\([+-][^ \t\n]+\\)" 1 font-lock-builtin-face)
            ("^[^ \t\n]+:.*" . font-lock-string-face)
            ("^\\[[1-9][0-9]*\\]" . font-lock-constant-face)))

    (minimal-emacs-autoload (minimal-shell minimal-shell-mode) "minimal-shell")

    (define-key global-map (kbd "<f1>") #'minimal-shell) ; I don't use F1 for help commands
    (add-hook 'shell-mode-hook #'minimal-shell-mode)))

;;; Show battery status on the mode line with `display-battery-mode'
(when minimal-laptop-p
  (minimal-emacs-configure
    (require 'battery)
    (setq battery-mode-line-format
          (cond
           ((eq battery-status-function #'battery-linux-proc-acpi)
	        "⏻ %b%p%%,%d°C ")
	       (battery-status-function
	        "⏻ %b%p%% ")))
    (display-battery-mode 1)))

(provide 'minimal-emacs-essentials)
