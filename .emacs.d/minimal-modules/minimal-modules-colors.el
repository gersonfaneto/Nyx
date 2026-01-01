(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them."
  (mapc #'disable-theme custom-enabled-themes))

(setq custom-safe-themes t)

(setq frame-background-mode minimal/default-background)

(defun minimal/load-theme ()
  "Load the default theme."
  (interactive)
  (setq gruvbox-material-style minimal/default-background
	gruvbox-material-contrast minimal/default-contrast)
  (load-theme minimal/default-theme))

(defun minimal/toggle-background ()
  "Switch between dark/light modes of the current color theme."
  (interactive)
  (setq frame-background-mode
        (if (eq frame-background-mode 'dark) 'light 'dark))
  (setq gruvbox-material-style frame-background-mode)
  (mapc 'frame-set-background-mode (frame-list))
  (load-theme minimal/default-theme))

(global-set-key (kbd "C-M-0") 'minimal/toggle-background)

(provide 'minimal-modules-colors)
