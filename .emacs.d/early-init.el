;; -*- lexical-binding: t; outline-regexp: ";;;"; eval: (local-set-key (kbd "C-c i") #'consult-outline); -*-

;; A foolish attempt on a minimal EMACS configuration, by @gersonfaneto.

;;; I hate that noise...
(setq ring-bell-function 'ignore)

;;; YOLO!
(setq warning-minimum-level :error
      warning-minimum-log-level :error)

;;; Squeezing the hell out of this one...
(defun harder-better-faster-stronger ()
  "Makes things faster by tweaking some settings O.o"
  ;; For garbage collection
  (setq gc-cons-threshold (* 100 1000 1000))
  ;; For LSPs
  (setq read-process-output-max (* 1024 1024))
  ;; https://github.com/hlissner/doom-emacs/blob/58af4aef56469f3f495129b4e7d947553f420fca/core/core.el#L200
  (unless (daemonp)
    (advice-add #'display-startup-echo-area-message :override #'ignore))
  ;; https://github.com/hlissner/doom-emacs/blob/58af4aef56469f3f495129b4e7d947553f420fca/core/core.el#L323
  (setq frame-inhibit-implied-resize t)
  ;; https://github.com/hlissner/doom-emacs/blob/58af4aef56469f3f495129b4e7d947553f420fca/core/core.el#L331
  (setq inhibit-compacting-font-caches t)
  ;; https://github.com/hlissner/doom-emacs/blob/58af4aef56469f3f495129b4e7d947553f420fca/core/core.el#L205
  (setq idle-update-delay 1.0)
  ;; Don't want a mode line while loading init.
  (setq-default mode-line-format nil))

;;; No default splash screen nor scratch buffer message
(setq inhibit-startup-screen t
      initial-scratch-message nil)

;;; No scrollbar by default
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;;; No nenubar by default
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;;; No toolbar by default
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;;; No tooltip by default
(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))

;;; Backup & Temporary files
(setq auto-save-default nil
      make-backup-files nil)

;;; Spacing & Indentation
(setq tab-width 2
      fill-column 100
      indent-tabs-mode nil)

;;; Don't open dialog boxes
(setq use-dialog-box nil
      use-file-dialog nil)

;;; Make EMACS a tidy less stubborn
(setq use-short-answers t
      compilation-ask-about-save nil
      confirm-nonexistent-file-or-buffer nil)

;;; Dired
(setq dired-listing-switches "-lhAX --group-directories-first")

;;; Line Numbers
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode)
  (setq display-line-numbers-type 'relative))

;;; UI Styling :: Defaults
(setq minimal/local-file            "~/.emacs.d/local.el"
      minimal/default-font          "Nova Nerd Font 12"
      minimal/current-theme         'doom-solarized-dark
      minimal/current-theme-dark    'doom-solarized-dark
      minimal/current-theme-light   'doom-solarized-light)

;;; UI Styling :: Overrides
(if (file-exists-p minimal/local-file)
    (load-file minimal/local-file))
