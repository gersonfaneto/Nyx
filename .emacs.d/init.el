(setq straight-repository-branch "develop")

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package straight
  :custom
  (straight-use-package-by-default t))

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

(setq
 inhibit-startup-screen t)

(setq tab-width 2
      indent-tabs-mode nil)

(setq make-backup-files nil)

(setq use-short-answers t
      confirm-nonexistent-file-or-buffer nil)

(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

(set-frame-font "GohuFont 14 Nerd Font 16" nil t)

(use-package gruber-darker-theme
  :straight t
  :config
  (load-theme 'gruber-darker t))

(global-set-key (kbd "C-0") 'delete-window)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-2") 'split-window-below)
(global-set-key (kbd "C-3") 'split-window-right)
(global-set-key (kbd "C-o") 'other-window)

(global-set-key (kbd "C-x p") 'previous-buffer)
(global-set-key (kbd "C-x n") 'next-buffer)

(global-set-key (kbd "C-.") 'duplicate-line)

(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c r") 'recompile)

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->")         'mc/mark-next-like-this)
  (global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
  (global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
  (global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this))


(use-package vertico
  :custom
  (vertico-scroll-margin 0)
  (vertico-count 8)
  (vertico-resize t)
  (vertico-cycle t)
  :init
  (vertico-mode)
  ;; (vertico-unobtrusive-mode)
  :config
  (setq read-file-name-completion-ignore-case t
	read-buffer-completion-ignore-case t
	completion-ignore-case t))

(use-package savehist
  :init
  (savehist-mode))

(use-package expand-region
  :ensure t
  :config 
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package smartparens
  :ensure t
  :hook (prog-mode . smartparens-mode)
  :custom
  (sp-escape-quotes-after-insert nil)
  :config
  (require 'smartparens-config))

(show-paren-mode t)

(use-package move-text
  :config
  (global-set-key (kbd "M-p") 'move-text-up)
  (global-set-key (kbd "M-n") 'move-text-down))

(use-package tab-jump-out
  :hook (prog-mode . tab-jump-out-mode))

(use-package whitespace-cleanup-mode
  :hook (prog-mode . whitespace-cleanup-mode))

(use-package magit
  :ensure t
  :init
  (bind-key (kbd "C-x g") 'magit-status))

(use-package direnv
  :config (direnv-mode))

(use-package haskell-mode :ensure t :mode "\\.hs\\'"
  :ensure t
  :hook ((haskell-mode . interactive-haskell-mode)
         (haskell-mode . turn-on-haskell-doc-mode)
         (haskell-mode . haskell-indent-mode)
         (haskell-mode . haskell-setup-outline-mode))
  :config
  (defun haskell-setup-outline-mode ()
    (make-local-variable 'outline-regexp)
    (setq outline-regexp "\\`\\|\\s-+\\S-")))

(custom-set-variables '(haskell-stylish-on-save t))

(when (file-exists-p "~/.emacs.d/custom.el")
  (setq custom-file "~/.emacs.d/custom.el")
  (load-file custom-file))
