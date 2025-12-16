;;; Dired file manager and minimal-dired.el extras
(minimal-emacs-configure
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t))

(setq dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso")

(setq dired-dwim-target t)

(setq dired-guess-shell-alist-user ; those are the suggestions for ! and & in Dired
      '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh *" "feh" "xdg-open")
        ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv *" "mpv" "xdg-open")
        (".*" "xdg-open")))

(minimal-emacs-configure
  (with-eval-after-load 'dired
    (setq dired-auto-revert-buffer #'dired-directory-changed-p) ; also see `dired-do-revert-buffer'
    (setq dired-make-directory-clickable t) ; Emacs 29.1
    (setq dired-free-space nil) ; Emacs 29.1
    (setq dired-mouse-drag-files t) ; Emacs 29.1

    (minimal-emacs-hook dired-mode-hook (dired-hide-details-mode hl-line-mode))

    ;; In Emacs 29 there is a binding for `repeat-mode' which lets you
    ;; repeat C-x C-j just by following it up with j.  For me, this is a
    ;; problem as j calls `dired-goto-file', which I often use.
    (define-key dired-jump-map (kbd "j") nil)))

(minimal-emacs-configure
  (with-eval-after-load 'dired
    (require 'dired-aux)

    (minimal-emacs-keybind dired-mode-map
      "C-+" #'dired-create-empty-file
      "M-s f" #'nil
      "C-<return>" #'dired-do-open ; Emacs 30
      "C-x v v" #'dired-vc-next-action) ; Emacs 28

    (setq dired-isearch-filenames 'dwim)
    (setq dired-create-destination-dirs 'ask) ; Emacs 27
    (setq dired-vc-rename-file t)             ; Emacs 27
    (setq dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir)))) ; Emacs 28
    (setq dired-create-destination-dirs-on-trailing-dirsep t) ; Emacs 29

    (require 'dired-x)

    (define-key dired-mode-map (kbd "I") #'dired-info)

    (setq dired-clean-up-buffers-too t)
    (setq dired-clean-confirm-killing-deleted-buffers t)
    (setq dired-x-hands-off-my-keys t)    ; easier to show the keys I use
    (setq dired-bind-man nil)
    (setq dired-bind-info nil)))

(minimal-emacs-configure
  (with-eval-after-load 'dired
    (require 'minimal-dired)
    (add-hook 'dired-mode #'minimal-dired-setup-imenu)
    (minimal-emacs-keybind dired-mode-map
      "i" #'minimal-dired-insert-subdir ; override `dired-maybe-insert-subdir'
      "/" #'minimal-dired-limit-regexp
      "C-c C-l" #'minimal-dired-limit-regexp
      "M-n" #'minimal-dired-subdirectory-next
      "C-c C-s" #'minimal-dired-search-flat-list
      "C-c C-n" #'minimal-dired-subdirectory-next
      "C-c C-p" #'minimal-dired-subdirectory-previous
      "M-s G" #'minimal-dired-grep-marked-files ; M-s g is `minimal-search-grep'
      "M-p" #'minimal-dired-subdirectory-previous)))

(minimal-emacs-configure
  (minimal-emacs-install dired-subtree)
  (with-eval-after-load 'dired
    (minimal-emacs-keybind dired-mode-map
      "<tab>" #'dired-subtree-toggle
      "TAB" #'dired-subtree-toggle
      "<backtab>" #'dired-subtree-remove
      "S-TAB" #'dired-subtree-remove)
    (setq dired-subtree-use-backgrounds nil)))

(minimal-emacs-configure
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))

(minimal-emacs-configure
  (with-eval-after-load 'image-dired
    (define-key image-dired-thumbnail-mode-map (kbd "<return>") #'image-dired-thumbnail-display-external)
    (setq image-dired-thumbnail-storage 'standard)
    (setq image-dired-external-viewer "xdg-open")
    (setq image-dired-thumb-size 80)
    (setq image-dired-thumb-margin 2)
    (setq image-dired-thumb-relief 0)
    (setq image-dired-thumbs-per-row 4)))

;;; Automatically preview Dired file at point (dired-preview.el)
;; One of my packages: <https://protesilaos.com/emacs>
(minimal-emacs-configure
  (minimal-emacs-install dired-preview)
  (with-eval-after-load 'dired
    (require 'dired-preview)
    (add-hook 'dired-mode-hook (lambda ()
                                 (when (string-match-p "Pictures" default-directory)
                                   (dired-preview-mode 1))))
    (define-key dired-mode-map (kbd "V") #'dired-preview-mode)

    (setq dired-preview-trigger-on-start nil)
    (setq dired-preview-max-size (* (expt 2 20) 10))
    (setq dired-preview-delay 0.5)
    (setq dired-preview-ignored-extensions-regexp
          (concat "\\."
                  "\\(gz\\|"
                  "zst\\|"
                  "tar\\|"
                  "xz\\|"
                  "rar\\|"
                  "zip\\|"
                  "iso\\|"
                  "epub"
                  "\\)"))

    (setq dired-preview-display-action-alist #'dired-preview-display-action-alist-below)))

;;; dired-like mode for the trash (trashed.el)
(minimal-emacs-configure
  (minimal-emacs-install trashed)
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(provide 'minimal-emacs-dired)
