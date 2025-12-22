;; -*- lexical-binding: t; outline-regexp: ";; ---"; eval: (local-set-key (kbd "C-c i") #'consult-outline); -*-

;; A foolish attempt on a minimal EMACS configuration, by @gersonfaneto.

;; --- Startup Optimizations ---
;; Suppress the visual bell for fewer distractions.
(setq ring-bell-function 'ignore)

;; Set minimum warning levels to errors to reduce noise during startup.
(setq warning-minimum-level :error
      warning-minimum-log-level :error)

;; Optimize garbage collection for faster startup by increasing the threshold.
(setq gc-cons-percentage 0.6
      gc-cons-threshold most-positive-fixnum)

;; Adjust garbage collection threshold after initialization is complete.
(add-hook 'after-init-hook
	  #'(lambda () (setq gc-cons-threshold (* 8 1024 1024))))

;; Enable quick package loading and defer package startup until later.
(setq package-quickstart t
      package-enable-at-startup nil)

;; Disable the startup echo area message if Emacs is not running as a daemon.
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

;; Prevent implied frame resizing during startup.
(setq frame-inhibit-implied-resize t)

;; Disable font cache compaction to potentially speed up startup.
(setq inhibit-compacting-font-caches t)

;; Reduce idle update delay for quicker UI responsiveness.
(setq idle-update-delay 1.0)

;; --- UI Customizations ---
;; Disable the mode line to simplify the interface.
;; (setq mode-line-format nil)

;; Disable the startup screen and initial scratch message for a cleaner start.
(setq inhibit-startup-screen t
      initial-scratch-message nil)

;; Turn off scroll bars, vertical scroll bars, menu bar, tool bar, and tooltips
;; to further minimize the UI.
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

;; --- Buffers Handling and Behavior ---
;; Disable auto-save and backup files to keep the environment cleaner.
(setq auto-save-default nil
      make-backup-files nil)

;; Set default indentation and line wrapping behavior.
(setq tab-width 2
      fill-column 100
      indent-tabs-mode nil)

;; Enable relative line numbers if Emacs version is recent enough.
;; (when (version<= "26.0.50" emacs-version)
;;   (global-display-line-numbers-mode)
;;   (setq display-line-numbers-type 'relative))

(add-hook 'prog-mode-hook
	  #'(lambda () (toggle-truncate-lines 1)))

;; --- Editor Interactions ---
;; Disable all interactions through dialogs.
(setq use-dialog-box nil
      use-file-dialog nil)

;; Use short answers for prompts and disable confirmation for certain actions.
(setq use-short-answers t
      compilation-ask-about-save nil
      confirm-nonexistent-file-or-buffer nil)

;; --- Dired UI and Behavior ---
;; Configure dired listing switches for a detailed, grouped view.
(setq dired-listing-switches "-lhAX --group-directories-first")

;; Configure Dired to kill the old buffer when opening a new one.
(setq dired-kill-when-opening-new-dired-buffer t)

;; Configure Dired to move files to trash upon deletion.
(setq delete-by-moving-to-trash t)

;; --- Custom File and Theme Definitions ---
;; Define paths and initial settings for custom files, font, and theme.
(setq minimal/local-file            "~/.emacs.d/local.el"
      minimal/default-font          "Recursive 10"
      minimal/default-theme         'solarized
      minimal/default-background    'dark)

;; Load local customizations if the file exists.
(if (file-exists-p minimal/local-file)
    (load-file minimal/local-file))

;; --- Custom File Loading ---
;; Path to the custom file.
(setq minimal/custom-file "~/.emacs.d/custom.el")

;; Create the custom file if it doesn't exist.
(if (not (file-exists-p minimal/custom-file))
    (make-empty-file minimal/custom-file))

;; Load custom file if it exists.
(when (file-exists-p minimal/custom-file)
  (setq custom-file minimal/custom-file)	;; Set Emacs's custom-file variable
  (load-file custom-file))			;; Load the custom file
