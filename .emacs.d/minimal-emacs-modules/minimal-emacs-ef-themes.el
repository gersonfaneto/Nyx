;;; The Ef (εὖ) themes

(minimal-emacs-configure
  (minimal-emacs-install ef-themes)

  (ef-themes-take-over-modus-themes-mode 1)

  (minimal-emacs-keybind global-map
    "<f5>" #'modus-themes-rotate
    "C-<f5>" #'modus-themes-select
    "M-<f5>" #'modus-themes-load-random)

  (setq modus-themes-variable-pitch-ui t
        modus-themes-mixed-fonts t
        modus-themes-to-rotate nil ; defaults to the return value of `modus-themes-get-themes'
        modus-themes-headings ; read the manual's entry of the doc string
        '((0 . (variable-pitch light 1.9))
          (1 . (variable-pitch light 1.8))
          (2 . (variable-pitch regular 1.7))
          (3 . (variable-pitch regular 1.6))
          (4 . (variable-pitch regular 1.5))
          (5 . (variable-pitch 1.4)) ; absence of weight means `bold'
          (6 . (variable-pitch 1.3))
          (7 . (variable-pitch 1.2))
          (agenda-date . (semilight 1.5))
          (agenda-structure . (variable-pitch light 1.9))
          (t . (variable-pitch 1.1))))

  (if (minimal-emacs-gnome-prefers-dark-p)
      (modus-themes-load-random 'dark)
    (modus-themes-load-random 'light)))

(minimal-emacs-comment
  (:eval nil)

  (add-to-list 'load-path "~/Git/Projects/modus-themes/")
  (add-to-list 'load-path "~/Git/Projects/ef-themes/")

  (require 'modus-themes)
  (require 'ef-themes)

  (ef-themes-take-over-modus-themes-mode 1)

  (minimal-emacs-keybind global-map
    "<f5>" #'modus-themes-rotate
    "C-<f5>" #'modus-themes-select
    "M-<f5>" #'modus-themes-load-random)

  (if (minimal-emacs-gnome-prefers-dark-p)
      (modus-themes-load-random 'dark)
    (modus-themes-load-random 'light)))

  (provide 'minimal-emacs-ef-themes)
