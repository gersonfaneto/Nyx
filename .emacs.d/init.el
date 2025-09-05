(setq straight-repository-branch "develop")

(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

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
 inhibit-startup-screen t
 initial-scratch-message "")

(setq tab-width 2
      indent-tabs-mode nil)

(setq make-backup-files nil)

(setq use-short-answers t
      confirm-nonexistent-file-or-buffer nil)

(setq dired-listing-switches "-lhAX --group-directories-first")

(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

(set-frame-font "mononoki 12" nil t)

(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "M-o") 'other-window)

(defun min/duplicate-line ()
  "Duplicates current line"
  (interactive)
  (let ((column (- (point) (point-at-bol)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

(global-set-key (kbd "C-.") 'min/duplicate-line)

(global-set-key (kbd "C-x C-p") 'previous-buffer)
(global-set-key (kbd "C-x C-n") 'next-buffer)
(global-set-key (kbd "C-x C-k") 'kill-current-buffer)

(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c r") 'recompile)

(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-gruvbox t))

(use-package doom-modeline
  :init (doom-modeline-mode)
  :custom (doom-modeline-icon (display-graphic-p)))

(use-package all-the-icons
  :if (display-graphic-p)
  :commands all-the-icons-install-fonts
  :config (unless (find-font (font-spec :name "all-the-icons"))
            (all-the-icons-install-fonts t)))

(use-package evil
  :straight t
  :init
  (setq evil-want-keybinding nil)
  (evil-mode 1)
  :config
  (setq evil-want-integration t
        evil-want-C-u-scroll t
        evil-shift-width 2
        evil-search-module 'evil-search)
  (evil-global-set-key 'normal (kbd "SPC f c") (lambda () (interactive)
						 (let ((default-directory user-emacs-directory))
						   (call-interactively 'find-file)))))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-lion
  :straight t
  :after evil
  :config
  (evil-lion-mode))

(use-package evil-surround
  :straight t
  :after evil
  :config
  (global-evil-surround-mode 1))

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
  :init (vertico-mode)
  :config (setq read-file-name-completion-ignore-case t
		read-buffer-completion-ignore-case t
		completion-ignore-case t))

(use-package savehist
  :init (savehist-mode))

(use-package expand-region
  :ensure t
  :config (global-set-key (kbd "C-=") 'er/expand-region))

(use-package smartparens
  :ensure t
  :hook (prog-mode . smartparens-mode)
  :custom (sp-escape-quotes-after-insert nil)
  :init (show-paren-mode t)
  :config (require 'smartparens-config))

(use-package tab-jump-out
  :hook (prog-mode . tab-jump-out-mode))

(use-package move-text
  :config
  (global-set-key (kbd "M-p") 'move-text-up)
  (global-set-key (kbd "M-n") 'move-text-down))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :init (pdf-tools-install :no-query))

(use-package magit
  :ensure t
  :init (bind-key (kbd "C-x g") 'magit-status))

(use-package whitespace-cleanup-mode
  :hook (prog-mode . whitespace-cleanup-mode))

(use-package direnv
  :config (direnv-mode))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

(use-package haskell-mode :ensure t :mode "\\.hs\\'"
  :ensure t
  :hook ((haskell-mode . interactive-haskell-mode)
         (haskell-mode . turn-on-haskell-doc-mode)
         (haskell-mode . haskell-setup-outline-mode))
  :config
  (defun haskell-setup-outline-mode ()
    (make-local-variable 'outline-regexp)
    (setq outline-regexp "\\`\\|\\s-+\\S-"))
  (setq haskell-stylish-on-save t))

(use-package go-mode
  :mode "\\.go\\'"
  :hook (before-save . gofmt-before-save)
  :custom (gofmt-command "goimports"))

(setq min/custom-file "~/.emacs.d/custom.el")

(if (not (file-exists-p min/custom-file))
    (make-empty-file min/custom-file))

(defun min/setup-frame (frame)
  "Setup fonts, theme, and modeline for new frames."
  (with-selected-frame frame
    (load-theme 'doom-gruvbox t)
    (set-frame-font "mononoki 12" nil t)
    (doom-modeline-mode 1)))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'min/setup-frame)
  (min/setup-frame (selected-frame)))

(when (file-exists-p min/custom-file)
  (setq custom-file min/custom-file)
  (load-file custom-file))
