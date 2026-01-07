(setq custom-safe-themes t)

(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them."
  (mapc #'disable-theme custom-enabled-themes))

(defun minimal/load-theme ()
  "Load the default theme using stored background."
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

(defun minimal/toggle-background ()
  "Switch between dark/light modes of the current color theme."
  (interactive)
  (setq frame-background-mode
        (if (eq frame-background-mode 'dark) 'light 'dark))
  (setq gruvbox-material-style frame-background-mode)
  (mapc #'frame-set-background-mode (frame-list))
  (minimal/write-background-to-state frame-background-mode)
  (load-theme minimal/default-theme t))

(global-set-key (kbd "C-M-0") 'minimal/toggle-background)

(provide 'minimal-modules-colors)
