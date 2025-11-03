;; -*- lexical-binding: t; -*-

;; A foolish attempt on a minimal EMACS configuration, by @gersonfaneto.

(setq ring-bell-function 'ignore)

(setq warning-minimum-level :error
      warning-minimum-log-level :error)

(setq gc-cons-percentage 0.6
      gc-cons-threshold most-positive-fixnum)

(add-hook 'after-init-hook
	  #'(lambda () (setq gc-cons-threshold (* 8 1024 1024))))

(setq package-quickstart t
      package-enable-at-startup nil)

(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

(setq frame-inhibit-implied-resize t)

(setq inhibit-compacting-font-caches t)

(setq idle-update-delay 1.0)

(setq mode-line-format nil)

(setq inhibit-startup-screen t
      initial-scratch-message nil)

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 0))

(when (fboundp 'vertical-scroll-bars)
  (vertical-scroll-bars 0))

(when (fboundp 'menu-bar-mode)
  (menu-bar-mode 0))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))

(when (fboundp 'tooltip-mode)
  (tooltip-mode 0))

(setq auto-save-default nil
      make-backup-files nil)

(setq tab-width 2
      fill-column 100
      indent-tabs-mode nil)

(setq use-dialog-box nil
      use-file-dialog nil)

(setq use-short-answers t
      compilation-ask-about-save nil
      confirm-nonexistent-file-or-buffer nil)

(setq dired-listing-switches "-lhAX --group-directories-first")

(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode)
  (setq display-line-numbers-type 'relative))

(setq minimal/local-file            "~/.emacs.d/local.el"
      minimal/default-font          "Nova Nerd Font 12"
      minimal/current-theme         'doom-solarized-dark
      minimal/current-theme-dark    'doom-solarized-dark
      minimal/current-theme-light   'doom-solarized-light)

(if (file-exists-p minimal/local-file)
    (load-file minimal/local-file))
