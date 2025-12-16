;;; The Doric themes

(minimal-emacs-configure
  (minimal-emacs-install doric-themes)

  (minimal-emacs-keybind global-map
    "<f5>" #'doric-themes-rotate
    "C-<f5>" #'doric-themes-select)

  (doric-themes-load-random
   (if (minimal-emacs-gnome-prefers-dark-p)
       'dark
     'light)))

;; For testing purposes
(minimal-emacs-comment
  (:eval nil)

  (add-to-list 'load-path "~/Git/Projects/doric-themes/")

  (require 'doric-themes)

  (minimal-emacs-keybind global-map
    "<f5>" #'doric-themes-rotate
    "C-<f5>" #'doric-themes-select)

  (doric-themes-load-random
   (if (minimal-emacs-gnome-prefers-dark-p)
       'dark
     'light)))

(provide 'minimal-emacs-doric-themes)
