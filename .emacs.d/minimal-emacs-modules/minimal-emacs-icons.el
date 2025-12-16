;;; Icons
(when minimal-display-graphic-p
  (minimal-emacs-configure
    (minimal-emacs-install nerd-icons)

    (minimal-emacs-install nerd-icons-completion)

    (nerd-icons-completion-mode 1)

    ;; By default, icons are shown in all sorts of completion prompts.
    ;; When those have different kinds of candidates, like files and
    ;; folders, the icons are helpful.  If all the candidates have the
    ;; same icon though, I prefer not to see any icon.
    (setq nerd-icons-completion-category-icons nil)

    (minimal-emacs-install nerd-icons-corfu)

    (with-eval-after-load 'corfu
      (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

    (minimal-emacs-install nerd-icons-dired)

    (add-hook 'dired-mode-hook #'nerd-icons-dired-mode)

    (minimal-emacs-install nerd-icons-xref)

    (with-eval-after-load 'xref
      (nerd-icons-xref-mode 1))

    (minimal-emacs-install nerd-icons-grep)

    (with-eval-after-load 'grep
      (when grep-use-headings
        (nerd-icons-grep-mode 1)))))

(provide 'minimal-emacs-icons)
