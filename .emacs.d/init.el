;; -*- lexical-binding: t; -*-

;; A foolish attempt on a minimal EMACS configuration, by @gersonfaneto.

(set-frame-font minimal/default-font nil t)

(defun minimal/scale-font (n)
  "With positive N, increase the font size; with negative N, decrease it; with N=0, reset to default height."
  (set-face-attribute 'default (selected-frame) :height
		      (cond ((> n 0) (+ (face-attribute 'default :height) 5))
			    ((< n 0) (- (face-attribute 'default :height) 5))
			    (t 100))))

(global-set-key (kbd "C-0") '(lambda nil (interactive) (minimal/scale-font  0)))
(global-set-key (kbd "C-=") '(lambda nil (interactive) (minimal/scale-font  1)))
(global-set-key (kbd "C--") '(lambda nil (interactive) (minimal/scale-font -1)))

(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "M-o") 'other-window)

(global-set-key (kbd "M-s s") 'flyspell-mode)

(global-set-key (kbd "M-g l") 'browse-url)

(defun minimal/duplicate-line ()
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

(global-set-key (kbd "C-.") 'minimal/duplicate-line)

(global-set-key (kbd "M-j") 'forward-paragraph)
(global-set-key (kbd "M-k") 'backward-paragraph)

(defun kill-all-buffers ()
  "Close all buffers."
  (interactive)
  (let ((lsp-restart 'ignore))
    (delete-other-windows)
    (save-some-buffers)
    (let
        ((kill-buffer-query-functions '()))
      (mapc 'kill-buffer (buffer-list)))))

(global-set-key (kbd "C-x C-p") 'previous-buffer)
(global-set-key (kbd "C-x C-n") 'next-buffer)
(global-set-key (kbd "C-x C-k") 'kill-current-buffer)
(global-set-key (kbd "C-x C-S-k") 'kill-all-buffers)

(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c r") 'recompile)

(setq straight-repository-branch "develop")

(setq package-archives '(("elpa"	.	"https://elpa.gnu.org/packages/")
                         ("melpa"	.	"https://melpa.org/packages/")
                         ("nongnu"	.	"https://elpa.nongnu.org/nongnu/")))

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

(setq use-package-verbose nil
      use-package-always-ensure t)

(use-package straight :ensure  nil
  :custom
  (straight-use-package-by-default t))

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(use-package doom-themes
  :init
  (load-theme minimal/current-theme t)
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-icon (display-graphic-p)))

(use-package all-the-icons
  :if
  (display-graphic-p)
  :commands
  all-the-icons-install-fonts
  :config
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t)))

(use-package which-key
  :config
  (which-key-mode))

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-count 8)
  (vertico-cycle t)
  (vertico-resize t)
  (vertico-scroll-margin 0)
  :config
  (setq completion-ignore-case t
	read-buffer-completion-ignore-case t
	read-file-name-completion-ignore-case t))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
	read-buffer-completion-ignore-case t
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :bind
  (("C-C C-x b" . 'consult-buffer)
   ("C-c C-x m" . 'consult-man)
   ("C-c C-x g" . 'consult-ripgrep)
   ("C-c C-x s" . 'consult-line)
   ("C-c C-x S" . 'consult-keep-lines)
   ("C-c C-x i" . 'consult-imenu)
   ("C-c C-x t" . 'consult-theme)))

(use-package multiple-cursors
  :bind
  ("C->" . 'mc/mark-next-like-this)
  ("C-<" . 'mc/mark-previous-like-this)
  ("C-;" . 'mc/skip-to-next-like-this)
  ("C-:" . 'mc/skip-to-previous-like-this)
  ("C-c C-<" . 'mc/mark-all-like-this)
  ("C-S-c C-S-c" . 'mc/edit-lines))

(use-package move-text
  :bind
  ("M-p" . 'move-text-up)
  ("M-n" . 'move-text-down))

(use-package expand-region
  :bind
  ("C-+" . 'er/expand-region))

(use-package smartparens
  :init
  (show-paren-mode t)
  :hook
  (prog-mode . smartparens-mode)
  :custom
  (sp-escape-quotes-after-insert nil)
  :config
  (require 'smartparens-config))

(use-package tab-jump-out
  :hook
  (prog-mode . tab-jump-out-mode))

(use-package magit
  :bind
  ("C-x g" . 'magit-status))

(use-package markdown-mode
  :init
  (setq markdown-command "multimarkdown")
  :mode
  ("README\\.md\\'" . gfm-mode)
  :bind
  (:map markdown-mode-map
        ("C-c C-e" . markdown-do)))

(use-package whitespace-cleanup-mode
  :hook
  (prog-mode . whitespace-cleanup-mode))

(use-package direnv
  :config
  (direnv-mode))

(use-package gptel
  :commands
  (gptel gptel-send)
  :bind (("C-c j" . gptel-menu)
         ("C-c C-g" . gptel-abort)
	 ("C-c <return>" . gptel-send)
	 ("C-c C-<return>" . gptel-menu)))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :bind
  (:map lsp-mode-map
	("C-c C-c a" . lsp-execute-code-action)
	("C-c C-c r" . lsp-rename)
	("C-c C-c k" . lsp-doc-glance)
	("C-c C-c f" . lsp-format-buffer)
	("C-c C-c d" . lsp-find-definition)
	("C-c C-c /" . lsp-find-references)
	("C-c C-c i" . lsp-organize-imports)
	("C-c C-c h" . lsp-describe-thing-at-point)))

(use-package lsp-ui
  :commands
  lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-diagnostics t))

(use-package treemacs
  :bind
  ("M-SPC" . treemacs)
  :bind
  (:map treemacs-mode-map
	("j" . treemacs-next-line)
	("k" . treemacs-previous-line)))

(use-package lsp-treemacs
  :commands
  lsp-treemacs-errors-list)

(use-package company
  :hook
  (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1))

(use-package flycheck
  :ensure t)

(use-package haskell-mode
  :mode
  "\\.hs\\'"
  :hook
  ((haskell-mode . interactive-haskell-mode)
   (haskell-mode . turn-on-haskell-doc-mode)
   (haskell-mode . haskell-setup-outline-mode))
  :config
  (setq haskell-stylish-on-save t)
  (defun haskell-setup-outline-mode ()
    (make-local-variable 'outline-regexp)
    (setq outline-regexp "\\`\\|\\s-+\\S-")))

(use-package nix-mode
  :mode
  "\\.nix\\'"
  :hook
  (nix-mode . lsp))

(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them."
  (mapc #'disable-theme custom-enabled-themes))

(setq custom-safe-themes t)

(defun minimal/toggle-theme ()
  "Toggle between light & dark variants of current theme."
  (interactive)
  (let ((new-theme (if (eq minimal/current-theme minimal/current-theme-dark)
		       minimal/current-theme-light
                     minimal/current-theme-dark)))
    (disable-theme minimal/current-theme)
    (load-theme new-theme t)
    (setq minimal/current-theme new-theme)
    (custom-set-variables `(minimal/current-theme ',new-theme))))

(global-set-key (kbd "C-c t") 'minimal/toggle-theme)

(defun minimal/setup-frame (frame)
  "Setup fonts, theme, and modeline for new frames."
  (with-selected-frame frame
    (load-theme minimal/current-theme t)
    (set-frame-font minimal/default-font nil t)))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'minimal/setup-frame)
  (minimal/setup-frame (selected-frame)))

(setq minimal/custom-file "~/.emacs.d/custom.el")

(if (not (file-exists-p minimal/custom-file))
    (make-empty-file minimal/custom-file))

(when (file-exists-p minimal/custom-file)
  (setq custom-file minimal/custom-file)
  (load-file custom-file))
