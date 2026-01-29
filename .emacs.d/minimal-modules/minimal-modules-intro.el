;;; minimal-modules-intro.el --- Minimal floating intro -*- lexical-binding: t; -*-

;; Prevent double loading
(defvar minimal-intro--loaded nil)
(when minimal-intro--loaded
  (cl-return-from load))
(setq minimal-intro--loaded t)

;; Disable built-in startup screens
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-splash-screen t)

(defvar minimal-intro--frame nil)
(defvar minimal-intro--buffer "*minimal-intro*")

(defun minimal-intro--should-show-p ()
  (and (display-graphic-p)
       (= (length command-line-args-left) 0)))

(defun minimal-intro--random-hint ()
  (pcase (random 3)
    (0 (list "SPC" " to start"))
    (1 (list "C-h" " for help"))
    (_ (list "C-x C-c" " to quit"))))

(defun minimal-intro--clear ()
  (when (frame-live-p minimal-intro--frame)
    (delete-frame minimal-intro--frame))
  (when (get-buffer minimal-intro--buffer)
    (kill-buffer minimal-intro--buffer)))

(defun minimal-intro--insert-centered (lines)
  "Insert LINES centered by manual padding (no reflow)."
  (let ((max-width (apply #'max (mapcar #'string-width lines))))
    (dolist (line lines)
      (let ((pad (/ (- max-width (string-width line)) 2)))
	(insert (make-string (max pad 0) ?\s))
	(insert line)
	(insert "\n")))))

(defun minimal-intro--show ()
  (when (minimal-intro--should-show-p)
    (let* ((buf (get-buffer-create minimal-intro--buffer))
	   (title (if (display-graphic-p)
		      "M I N I M Λ L"
		    "M I N I M A L"))
	   (hint (minimal-intro--random-hint)))

      ;; Prepare buffer
      (with-current-buffer buf
	(erase-buffer)
	(setq-local mode-line-format nil
		    cursor-type nil
		    truncate-lines t
		    word-wrap nil
		    show-trailing-whitespace nil)
	(setq-local buffer-read-only nil)

	(minimal-intro--insert-centered
	 (list
	  title
	  (propertize "by @gersonfaneto" 'face 'font-lock-comment-face)
	  ""
	  (concat
	   (propertize (car hint) 'face 'font-lock-comment-face)
	   (cadr hint))))

	(read-only-mode 1))

      ;; Create child frame
      (setq minimal-intro--frame
	    (make-frame
	     `((parent-frame . ,(selected-frame))
	       (minibuffer . nil)
	       (undecorated . t)
	       (no-accept-focus . t)
	       (no-focus-on-map . t)
	       (border-width . 0)
	       (internal-border-width . 24)
	       (child-frame-border-width . 0)
	       (vertical-scroll-bars . nil)
	       (horizontal-scroll-bars . nil)
	       (menu-bar-lines . 0)
	       (tool-bar-lines . 0)
	       (left-fringe . 0)
	       (right-fringe . 0)
	       (line-spacing . 2)
	       (width . 20)
	       (height . 8))))

      (set-window-buffer (frame-root-window minimal-intro--frame) buf)

      ;; Center child frame relative to monitor workarea (GTK/PGTK correct)
      (let* ((parent (selected-frame))
	     (attrs (frame-monitor-attributes parent))
	     (workarea (alist-get 'workarea attrs))
	     (wx (nth 0 workarea))
	     (wy (nth 1 workarea))
	     (ww (nth 2 workarea))
	     (wh (nth 3 workarea))
	     (fw (frame-pixel-width minimal-intro--frame))
	     (fh (frame-pixel-height minimal-intro--frame)))
	(set-frame-position
	 minimal-intro--frame
	 (+ wx (/ (- ww fw) 2))
	 (+ wy (/ (- wh fh) 2))))

      ;; Optical centering tweak (shift up ~½ line)
      (let* ((char-height (frame-char-height minimal-intro--frame)))
	(set-frame-position
	 minimal-intro--frame
	 (frame-parameter minimal-intro--frame 'left)
	 (- (frame-parameter minimal-intro--frame 'top)
	    (/ char-height 2))))

      ;; Clear intro on first meaningful interaction
      (dolist (hook '(pre-command-hook
		      window-size-change-functions
		      buffer-list-update-hook))
	(add-hook hook #'minimal-intro--clear)))))

(add-hook 'emacs-startup-hook #'minimal-intro--show)

(provide 'minimal-modules-intro)
;;; minimal-intro.el ends here
