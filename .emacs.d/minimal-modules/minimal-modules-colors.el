(setq custom-safe-themes t)

(defvar minimal/script-background nil
  "Non-nil when background change originates from external script.")

(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them."
  (mapc #'disable-theme custom-enabled-themes))

(defun minimal/load-theme ()
  "Load the default theme respecting current background."
  (interactive)
  (setq gruvbox-material-style frame-background-mode
        gruvbox-material-contrast minimal/default-contrast)
  (load-theme minimal/default-theme t))

(defun minimal/write-background-to-state (bg)
  "Persist BG ('dark or 'light) to state file."
  (let ((dir (file-name-directory minimal/state-colors-file)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (with-temp-file minimal/state-colors-file
      (insert (format "{ \"bg\": \"%s\" }\n" bg)))))

(defun minimal/apply-background (bg)
  "Apply BG ('dark or 'light) internally without side effects."
  (setq frame-background-mode bg
        gruvbox-material-style bg)
  (mapc #'frame-set-background-mode (frame-list))
  (load-theme minimal/default-theme t))

(defun minimal/toggle-background ()
  "Toggle dark/light background and propagate the change."
  (interactive)
  (let ((next (if (eq frame-background-mode 'dark) 'light 'dark)))
    (minimal/apply-background next)
    (minimal/write-background-to-state next)
    (when (executable-find "background")
      (start-process "background" nil "background"
                     (symbol-name next)))))

(defun minimal/set-background-from-script (bg)
  "Set background from external script without recursion."
  (let ((minimal/script-background t))
    (minimal/apply-background bg)))

(global-set-key (kbd "C-M-0") 'minimal/toggle-background)

(provide 'minimal-modules-colors)
