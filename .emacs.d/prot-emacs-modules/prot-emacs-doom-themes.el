;;; The Doom Themes

(prot-emacs-configure
  ;; Install `doom-themes' from the package repository.
  (prot-emacs-install doom-themes)

  ;; Override some defaults.
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)

  ;; This make sure all faces are cleared before loading a new theme,
  ;; making sure we don't have any unexpected colors popping up.
  (defadvice load-theme (before clear-previous-themes activate)
    "Clear existing theme settings instead of layering them."
    (mapc #'disable-theme custom-enabled-themes))

  ;; Load the custom themes from the local storage.
  (add-to-list 'custom-theme-load-path
               (expand-file-name "themes" user-emacs-directory))
  
  ;; Finally, load the dark theme by default.
  (load-theme 'doom-gruvbox))

(provide 'prot-emacs-doom-themes)
