;; -*- lexical-binding: t; outline-regexp: ";;;"; eval: (local-set-key (kbd "C-c i") #'consult-outline); -*-

;; A foolish attempt on a minimal EMACS configuration, by @better.

;;; In the beginning...
(harder-better-faster-stronger)

;;; Backup & Temporary files.
(setq auto-save-default nil
      make-backup-files nil)

;;; Spacing & Indentation 
(setq tab-width 2
      fill-column 100
      indent-tabs-mode nil)

;;; Make EMACS a tidy less stubborn
(setq use-short-answers t
      compilation-ask-about-save nil
      confirm-nonexistent-file-or-buffer nil)

;;; Dired
(setq dired-listing-switches "-lhAX --group-directories-first")

;;; Line Numbers 
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode)
  (setq display-line-numbers-type 'relative))

;;; UI Styling :: Defaults
(setq minimal/local-file          "~/.emacs.d/local.el"
      minimal/default-font        "Nova Nerd Font 12"
      minimal/current-theme       'doom-solarized-dark
      minimal/current-theme-dark  'doom-solarized-dark
      minimal/current-theme-light 'doom-solarized-light)

;;; UI Styling :: Overrides
(if (file-exists-p minimal/local-file)
    (load-file minimal/local-file))

;;; Font Settings
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

;;; Bindings :: Managing Windows
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "M-o") 'other-window)

;;; Spell Checking
(global-set-key (kbd "M-s s") 'flyspell-mode)

;;; Quick URLs
(global-set-key (kbd "M-g l") 'browse-url)

;;; Custom :: Duplicate Line
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

;;; Bindings :: Managing Buffers
(global-set-key (kbd "C-x C-p") 'previous-buffer)
(global-set-key (kbd "C-x C-n") 'next-buffer)
(global-set-key (kbd "C-x C-k") 'kill-current-buffer)

;;; Bindings :: Compilation
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c r") 'recompile)

;;; Packages :: Bootstrap
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

;;; Packages :: Server
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

;;; Packages :: DOOM Themes
(use-package doom-themes
  :init
  (load-theme minimal/current-theme t)
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t))

;;; Packages :: DOOM Modeline
(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-icon (display-graphic-p)))

;;; Packages :: Icons
(use-package all-the-icons
  :if
  (display-graphic-p)
  :commands
  all-the-icons-install-fonts
  :config
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t)))

;;; Packages :: Which-Key
(use-package which-key
  :config
  (which-key-mode))

;;; Packages :: Vertico
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

;;; Packages :: Marginalia
(use-package marginalia
  :init
  (marginalia-mode))

;;; Packages :: Orderless
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
	read-buffer-completion-ignore-case t
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

;;; Packages :: Consult
(use-package consult)

;;; Packages :: Multi-Cursors
(use-package multiple-cursors
  :bind
  ("C->" . 'mc/mark-next-like-this)
  ("C-<" . 'mc/mark-previous-like-this)
  ("C-;" . 'mc/skip-to-next-like-this)
  ("C-:" . 'mc/skip-to-previous-like-this)
  ("C-c C-<" . 'mc/mark-all-like-this)
  ("C-S-c C-S-c" . 'mc/edit-lines))

;;; Packages :: Move-Text
(use-package move-text
  :bind
  ("M-p" . 'move-text-up)
  ("M-n" . 'move-text-down))

;;; Packages :: Expand-Region
(use-package expand-region
  :bind
  ("C-+" . 'er/expand-region))

;;; Packages :: Smartparens
(use-package smartparens
  :init
  (show-paren-mode t)
  :hook
  (prog-mode . smartparens-mode)
  :custom
  (sp-escape-quotes-after-insert nil)
  :config
  (require 'smartparens-config))

;;; Packages :: Tab-Jump-Out
(use-package tab-jump-out
  :hook
  (prog-mode . tab-jump-out-mode))

;;; Packages :: Magit
(use-package magit
  :bind
  ("C-x g" . 'magit-status))

;;; Packages :: Markdown-Mode
(use-package markdown-mode
  :init
  (setq markdown-command "multimarkdown")
  :mode
  ("README\\.md\\'" . gfm-mode)
  :bind
  (:map markdown-mode-map
        ("C-c C-e" . markdown-do)))

;;; Packages :: Whitespace-Cleanup-Mode
(use-package whitespace-cleanup-mode
  :hook
  (prog-mode . whitespace-cleanup-mode))

;;; Packages :: Direnv
(use-package direnv
  :config
  (direnv-mode))

;;; Packages :: LSP-Mode
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l"))

;;; Packages :: LSP-UI
(use-package lsp-ui
  :commands
  lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-diagnostics t))

;;; Packages :: LSP-Treemmuacs
(use-package lsp-treemacs
  :commands
  lsp-treemacs-errors-list)

;;; Packages :: Company
(use-package company
  :hook
  (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1))

;;; Packages :: Haskell-Mode
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

;;; UI :: Cleanup Colors 
(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them."
  (mapc #'disable-theme custom-enabled-themes))

;;; UI :: Trust!
(setq custom-safe-themes t)

;;; UI :: Toggle Background
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

;;; UI :: Bravo!
(defun minimal/setup-frame (frame)
  "Setup fonts, theme, and modeline for new frames."
  (with-selected-frame frame
    (load-theme minimal/current-theme t)
    (set-frame-font minimal/default-font nil t)))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'minimal/setup-frame)
  (minimal/setup-frame (selected-frame)))

;;; Don't touch my stuff!
(setq minimal/custom-file "~/.emacs.d/custom.el")

(if (not (file-exists-p minimal/custom-file))
    (make-empty-file minimal/custom-file))

(when (file-exists-p minimal/custom-file)
  (setq custom-file minimal/custom-file)
  (load-file custom-file))
