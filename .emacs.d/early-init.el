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

;;; No default splash screen nor scratch buffer message.
(setq inhibit-startup-screen t
      initial-scratch-message nil)

;;; No scrollbar by default.
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;;; No nenubar by default.
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;;; No toolbar by default.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;;; No tooltip by default.
(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))

;;; A nice and minimal welcome buffer.
(defun minimal/show-welcome-buffer ()
  "Show *Welcome* buffer."
  (with-current-buffer (get-buffer-create "*Welcome*")
    (setq truncate-lines t)
    (minimal/refresh-welcome-buffer)
    (setq cursor-type nil)
    (read-only-mode +1)
    (add-hook 'window-size-change-functions
	      (lambda (_frame)
		(minimal/refresh-welcome-buffer)) nil t)
    (add-hook 'window-configuration-change-hook
	      #'minimal/refresh-welcome-buffer nil t)
    (switch-to-buffer (current-buffer))
    (local-set-key (kbd "q") 'kill-this-buffer)))

(defun minimal/refresh-welcome-buffer ()
  "Refresh welcome buffer content for WINDOW."
  (when-let* ((inhibit-read-only t)
	      (welcome-buffer (get-buffer "*Welcome*"))
	      (window (get-buffer-window welcome-buffer))
	      (image-path "~/.emacs.d/logo.png")
	      (image (create-image image-path nil nil :max-height 300))
	      (image-height (cdr (image-size image)))
	      (image-width (car (image-size image)))
	      (top-margin (floor (/ (- (window-height window) image-height) 2)))
	      (left-margin (floor (/ (- (window-width window) image-width) 2)))
	      (title "Welcome to Emacs"))
    (with-current-buffer welcome-buffer
      (erase-buffer)
      ;; (setq mode-line-format nil)
      (goto-char (point-min))
      (insert (make-string top-margin ?\n))
      (insert (make-string left-margin ?\ ))
      (insert-image image)
      (insert "\n\n\n")
      (insert (make-string (floor (/ (- (window-width window) (* (string-width title) 1.2)) 2)) ?\ ))
      (insert (propertize title 'face '(:height 1.2))))))

(when (< (length command-line-args) 2)
  (add-hook 'emacs-startup-hook (lambda ()
				  (when (display-graphic-p)
				    (minimal/show-welcome-buffer)))))
