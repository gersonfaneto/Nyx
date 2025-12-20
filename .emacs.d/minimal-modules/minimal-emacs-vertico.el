;;; Vertical completion layout (vertico)
(minimal-emacs-configure
 (minimal-emacs-install vertico)

 (setq vertico-scroll-margin 0)
 (setq vertico-count 5)
 (setq vertico-resize t)
 (setq vertico-cycle t)

 (with-eval-after-load 'rfn-eshadow
   ;; This works with `file-name-shadow-mode' enabled.  When you are in
   ;; a sub-directory and use, say, `find-file' to go to your home '~/'
   ;; or root '/' directory, Vertico will clear the old path to keep
   ;; only your current input.
   (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

 (vertico-mode 1))

;;; Custom tweaks for vertico (minimal-vertico.el)
(minimal-emacs-configure
  (require 'minimal-vertico)
  (setq vertico-multiform-commands
        `(("consult-\\(.*\\)?\\(find\\|grep\\|ripgrep\\)" ,@minimal-vertico-multiform-maximal)))
  (setq vertico-multiform-categories
        `(;; Maximal
          (embark-keybinding ,@minimal-vertico-multiform-maximal)
          (multi-category ,@minimal-vertico-multiform-maximal)
          (consult-location ,@minimal-vertico-multiform-maximal)
          (imenu ,@minimal-vertico-multiform-maximal)
          (theme ,@minimal-vertico-multiform-maximal)
          (unicode-name ,@minimal-vertico-multiform-maximal)
          ;; Minimal
          (file ,@minimal-vertico-multiform-minimal
                (vertico-sort-function . vertico-sort-directories-first))
          (t ,@minimal-vertico-multiform-minimal)))

  (vertico-multiform-mode 1)

  (minimal-emacs-keybind vertico-map
    "<left>" #'backward-char
    "<right>" #'forward-char
    "TAB" #'minimal-vertico-private-complete
    "DEL" #'vertico-directory-delete-char
    "M-DEL" #'vertico-directory-delete-word
    "M-," #'vertico-quick-insert
    "M-." #'vertico-quick-exit)
  (minimal-emacs-keybind vertico-multiform-map
    "RET" #'minimal-vertico-private-exit
    "<return>" #'minimal-vertico-private-exit
    "C-n" #'minimal-vertico-private-next
    "<down>" #'minimal-vertico-private-next
    "C-p" #'minimal-vertico-private-previous
    "<up>" #'minimal-vertico-private-previous
    "C-l" #'vertico-multiform-vertical))

(provide 'minimal-emacs-vertico)
