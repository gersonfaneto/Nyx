;; -*- lexical-binding: t; outline-regexp: ";; ---"; eval: (local-set-key (kbd "C-c i") #'consult-outline); -*-

;; A foolish attempt on a minimal EMACS configuration, by @gersonfaneto.

;; --- Font Scaling Functions ---
(defun minimal/scale-font (n)
  "With positive N, increase the font size; with negative N, decrease it; with N=0, reset to default height."
  (set-face-attribute 'default (selected-frame) :height
                      (cond ((> n 0) (+ (face-attribute 'default :height) 5))
                            ((< n 0) (- (face-attribute 'default :height) 5))
                            (t 100)))) ;; Reset to a standard size (adjust 100 if needed)

;; Define keybindings for font scaling.
(global-set-key (kbd "C-0") '(lambda nil (interactive) (minimal/scale-font  0))) ;; Reset font size
(global-set-key (kbd "C-=") '(lambda nil (interactive) (minimal/scale-font  1))) ;; Increase font size
(global-set-key (kbd "C--") '(lambda nil (interactive) (minimal/scale-font -1))) ;; Decrease font size

;; --- Window Management Keybindings ---
(global-set-key (kbd "M-0") 'delete-window) ;; Delete the current window
(global-set-key (kbd "M-1") 'delete-other-windows) ;; Delete all windows except the current one
(global-set-key (kbd "M-2") (lambda () (interactive) (split-window-below) (other-window 1))) ;; Split window below and switch to the new window
(global-set-key (kbd "M-3") (lambda () (interactive) (split-window-right) (other-window 1))) ;; Split window to the right and switch to the new window
(global-set-key (kbd "M-o") 'other-window) ;; Cycle to the next window

;; --- Editing and Navigation Keybindings ---
(global-set-key (kbd "M-s s") 'flyspell-mode)       ;; Toggle spell checking
(global-set-key (kbd "M-g l") 'browse-url)          ;; Browse URL at point

;; Function to duplicate the current line.
(defun minimal/duplicate-line ()
  "Duplicates current line"
  (interactive)
  (let ((column (- (point) (point-at-bol))) ;; Store current column
        (line (let ((s (thing-at-point 'line t))) ;; Get current line content
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1) ;; Move to end of line
    (newline)            ;; Insert a newline
    (insert line)        ;; Insert the duplicated line
    (move-beginning-of-line 1) ;; Move back to the start of the buffer
    (forward-char column)))   ;; Restore original column

(global-set-key (kbd "C-.") 'minimal/duplicate-line) ;; Duplicate current line

(global-set-key (kbd "M-j") 'forward-paragraph)       ;; Move to next paragraph
(global-set-key (kbd "M-k") 'backward-paragraph)      ;; Move to previous paragraph

;; --- Buffer Management ---
(defun kill-all-buffers ()
  "Close all buffers."
  (interactive)
  ;; Temporarily ignore LSP restart to prevent interference
  (let ((lsp-restart 'ignore))
    (delete-other-windows) ;; Close all but the active window
    (save-some-buffers)    ;; Prompt to save modified buffers
    (let
        ((kill-buffer-query-functions '())) ;; Disable confirmation for killing buffers
      (mapc 'kill-buffer (buffer-list))))) ;; Kill all listed buffers

;; Keybindings for buffer navigation and management.
(global-set-key (kbd "C-x C-p") 'previous-buffer)     ;; Previous buffer
(global-set-key (kbd "C-x C-n") 'next-buffer)         ;; Next buffer
(global-set-key (kbd "C-x C-k") 'kill-current-buffer) ;; Kill current buffer
(global-set-key (kbd "C-x C-S-k") 'kill-all-buffers)  ;; Kill all buffers

;; --- Compilation Keybindings ---
(global-set-key (kbd "C-c c") 'compile)             ;; Run compilation
(global-set-key (kbd "C-c r") 'recompile)           ;; Recompile project

;; --- Straight.el Configuration ---
;; Specify the branch for straight.el.
(setq straight-repository-branch "develop")

;; Configure package archives for package installation.
(setq package-archives '(("elpa"	.	"https://elpa.gnu.org/packages/")
                         ("melpa"	.	"https://melpa.org/packages/")
                         ("nongnu"	.	"https://elpa.nongnu.org/nongnu/")))

;; Bootstrap straight.el if it's not already installed.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    ;; Download and evaluate the straight.el bootstrap script.
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage)) ;; Load straight.el after bootstrapping

;; Enable use-package and set its behavior.
(straight-use-package 'use-package)
(setq use-package-verbose nil             ;; Suppress verbose output from use-package
      use-package-always-ensure t)         ;; Automatically install packages if not present

;; Configure straight.el itself.
(use-package straight :ensure  nil
  :custom
  (straight-use-package-by-default t)) ;; Ensure all packages are managed by straight.el

;; --- Server Mode ---
;; Start Emacs server if not already running, for emacsclient.
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

;; --- Themes and Appearance ---
;; Load doom-themes and configure them.
(use-package doom-themes
  :init
  (load-theme minimal/current-theme t) ;; Load the initial theme
  :custom
  (doom-themes-enable-bold t)          ;; Enable bold font in themes
  (doom-themes-enable-italic t))       ;; Enable italic font in themes

;; Configure doom-modeline for the status bar.
(use-package doom-modeline
  :init
  (doom-modeline-mode 1)               ;; Enable doom-modeline
  :custom
  (doom-modeline-icon (display-graphic-p))) ;; Show icons if Emacs is running graphically

;; Install all-the-icons if in graphical mode.
(use-package all-the-icons
  :if
  (display-graphic-p) ;; Only load if Emacs has a graphical interface
  :commands
  all-the-icons-install-fonts
  :config
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t))) ;; Install icons if not found

;; --- UI Enhancements ---
;; Enable which-key to show keybindings.
(use-package which-key
  :config
  (which-key-mode))

;; Configure Vertico for improved completion UI.
(use-package vertico
  :init
  (vertico-mode)                       ;; Enable Vertico mode
  :custom
  (vertico-count 8)                    ;; Number of items to display
  (vertico-cycle t)                    ;; Cycle through completions
  (vertico-resize t)                   ;; Resize Vertico buffer
  (vertico-scroll-margin 0)            ;; Margin for scrolling
  :config
  ;; Case-insensitive completion for various prompts.
  (setq completion-ignore-case t
	read-buffer-completion-ignore-case t
	read-file-name-completion-ignore-case t))

;; Enable marginalia for annotations in the minibuffer.
(use-package marginalia
  :init
  (marginalia-mode))

;; Configure orderless for advanced completion styles.
(use-package orderless
  :config
  (setq completion-styles '(orderless basic) ;; Use orderless and basic styles
	read-buffer-completion-ignore-case t
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion))))) ;; File completion optimization

;; --- Navigation Enhancements ---
(use-package projectile
  :bind
  ("C-c f" . projectile-find-file)
  ("C-c d" . projectile-find-dir)
  :commands
  (projectile-find-file))

(use-package dirvish :after dired
  :bind
  (:map dired-mode-map
	("?" . dirvish-dispatch))
  :init
  (dirvish-override-dired-mode))

;; --- Consult Package ---
;; Enhance search and navigation with Consult.
(use-package consult
  :bind
  (("C-C C-x b" . 'consult-buffer)      ;; List and switch buffers
   ("C-c C-x m" . 'consult-man)         ;; Search man pages
   ("C-c C-x g" . 'consult-ripgrep)     ;; Search project with ripgrep
   ("C-c C-x s" . 'consult-line)        ;; Search lines across buffers
   ("C-c C-x S" . 'consult-keep-lines)  ;; Keep only selected lines
   ("C-c C-x i" . 'consult-imenu)       ;; Navigate code symbols
   ("C-c C-x t" . 'consult-theme)))     ;; Switch themes

;; --- Editing Enhancements ---
;; Multiple cursors for editing multiple places simultaneously.
(use-package multiple-cursors
  :bind
  ("C->" . 'mc/mark-next-like-this)   ;; Mark next similar occurrence
  ("C-<" . 'mc/mark-previous-like-this) ;; Mark previous similar occurrence
  ("C-;" . 'mc/skip-to-next-like-this)  ;; Skip to next similar occurrence
  ("C-:" . 'mc/skip-to-previous-like-this) ;; Skip to previous similar occurrence
  ("C-c C-<" . 'mc/mark-all-like-this)  ;; Mark all similar occurrences
  ("C-S-c C-S-c" . 'mc/edit-lines))     ;; Edit multiple lines

;; Move text up/down.
(use-package move-text
  :bind
  ("M-p" . 'move-text-up)             ;; Move line/region up
  ("M-n" . 'move-text-down))          ;; Move line/region down

;; Whitespace cleanup for programming modes.
(use-package whitespace-cleanup-mode
  :hook
  (prog-mode . whitespace-cleanup-mode)) ;; Enable cleanup in programming modes

;; Expand region for selecting increasingly larger code blocks.
(use-package expand-region
  :bind
  ("C-+" . 'er/expand-region))       ;; Expand region

;; Smartparens for automatic parenthesis/quote matching and insertion.
(use-package smartparens
  :init
  (show-paren-mode t)                 ;; Highlight matching parentheses
  :hook
  (prog-mode . smartparens-mode)      ;; Enable smartparens in programming modes
  :custom
  (sp-escape-quotes-after-insert nil) ;; Do not escape quotes after insertion
  :config
  (require 'smartparens-config))

;; Tab jump out functionality for programming modes.
(use-package tab-jump-out
  :hook
  (prog-mode . tab-jump-out-mode))    ;; Enable tab-jump-out in programming modes

(delete-selection-mode 1) ;; Deletes text between the point and the mark when a selection is active.

;; --- Version Control ---
;; Magit for Git integration.
(use-package magit
  :bind
  ("C-x g" . 'magit-status))          ;; Open Magit status buffer

;; --- Environment ----
;; Direnv for managing environment variables in projects.
(use-package direnv
  :config
  (direnv-mode))                      ;; Enable direnv mode

;; --- AI Assistance ---

;; GPTel for interacting with AI models (Gemini).
(use-package gptel
  :commands
  (gptel gptel-send)                  ;; Commands for GPTel
  :config
  ;; Configure GPTel to use Gemini and stream responses.
  (setq gptel-model 'gemini-2.5-flash-lite
	gptel-backend (gptel-make-gemini "Gemini"
			:key GEMINI_API_KEY ;; Ensure GEMINI_API_KEY is set in your environment or Emacs variables
			:stream t))
  :bind (("C-c j" . gptel-menu)        ;; Open GPTel menu
         ("C-c C-g" . gptel-abort)     ;; Abort current GPTel request
	 ("C-c <return>" . gptel-send)  ;; Send prompt to GPTel
	 ("C-c C-<return>" . gptel-menu))) ;; Send prompt and show menu

;; --- Language Server Protocol (LSP) Configuration ---
;; Core LSP mode setup.
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")     ;; Set prefix for LSP commands
  :bind
  (:map lsp-mode-map
	("C-c C-c a" . lsp-execute-code-action) ;; Execute code actions
	("C-c C-c r" . lsp-rename)             ;; Rename symbol
	("C-c C-c k" . lsp-doc-glance)         ;; Show documentation glance
	("C-c C-c f" . lsp-format-buffer)      ;; Format entire buffer
	("C-c C-c d" . lsp-find-definition)    ;; Find definition of symbol
	("C-c C-c /" . lsp-find-references)    ;; Find references to symbol
	("C-c C-c i" . lsp-organize-imports)   ;; Organize imports
	("C-c C-c h" . lsp-describe-thing-at-point))) ;; Describe symbol at point

;; LSP UI enhancements (diagnostics, code annotations).
(use-package lsp-ui
  :commands
  lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t)          ;; Enable documentation display
  (setq lsp-ui-doc-position 'at-point) ;; Position docs at point
  (setq lsp-ui-sideline-enable t)     ;; Enable sideline for diagnostics
  (setq lsp-ui-sideline-show-diagnostics t)) ;; Show diagnostics in the sideline

;; --- Tree-sitter and Treemacs ---
;; Treemacs for a file explorer and project navigation.
(use-package treemacs
  :bind
  ("M-SPC" . treemacs)                  ;; Toggle treemacs visibility
  :bind
  (:map treemacs-mode-map
	("j" . treemacs-next-line)        ;; Move down in treemacs
	("k" . treemacs-previous-line)))   ;; Move up in treemacs

;; LSP integration with Treemacs (e.g., for error lists).
(use-package lsp-treemacs
  :commands
  lsp-treemacs-errors-list)            ;; Show LSP errors in treemacs

;; --- Completion-at-Point ---
;; Company mode for code completion.
(use-package company
  :hook
  (prog-mode . company-mode)            ;; Enable company-mode in programming modes
  :config
  (setq company-idle-delay 0.1         ;; Delay before showing completion candidates
        company-minimum-prefix-length 1)) ;; Minimum characters to trigger completion

;; Flycheck for syntax checking.
(use-package flycheck
  :ensure t)                           ;; Ensure flycheck is installed

;; --- Markdown Mode ---
(use-package markdown-mode
  :init
  (setq markdown-command "multimarkdown") ;; Set the command for Markdown processing
  :mode
  ("README\\.md\\'" . gfm-mode)       ;; Use GFM mode for README.md files
  :bind
  (:map markdown-mode-map
        ("C-c C-e" . markdown-do)))     ;; Export Markdown

;; --- Haskell Mode ---
(use-package haskell-mode
  :mode
  "\\.hs\\'"                            ;; Apply to .hs files
  :hook
  ((haskell-mode . interactive-haskell-mode) ;; Enable interactive Haskell mode
   (haskell-mode . turn-on-haskell-doc-mode) ;; Enable Haskell doc mode
   (haskell-mode . haskell-setup-outline-mode)) ;; Setup outline for Haskell
  :config
  (setq haskell-stylish-on-save t)     ;; Use stylish-haskell on save
  ;; Custom function to set outline regexp for Haskell.
  (defun haskell-setup-outline-mode ()
    "Set up outline-regexp for Haskell mode."
    (make-local-variable 'outline-regexp)
    (setq outline-regexp "\\`\\|\\s-+\\S-")))

;; --- Nix Mode ---
;; Configuration for Nix language files.
(use-package nix-mode
  :mode
  "\\.nix\\'"                           ;; Apply to .nix files
  :hook
  (nix-mode . lsp))                     ;; Enable LSP for Nix mode

;; --- C/C++ Mode ---
(use-package cc-mode :ensure nil
  :hook
  (c-mode . lsp))

;; --- Lua Mode ---
(use-package lua-mode
  :hook
  (lua-mode . lsp))

;; --- Player Mode ---
(use-package ready-player
  :custom
  (ready-player-my-media-collection-location "~/Music/")
  :config
  (ready-player-mode 1))

;; --- Theme Toggling ---
;; Advice to clear previous themes before loading a new one.
(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them."
  (mapc #'disable-theme custom-enabled-themes))

;; Allow safe custom themes.
(setq custom-safe-themes t)

;; Function to toggle between light and dark variants of the current theme.
(defun minimal/toggle-theme ()
  "Toggle between light & dark variants of current theme."
  (interactive)
  (let ((new-theme (if (eq minimal/current-theme minimal/current-theme-dark)
		       minimal/current-theme-light
                     minimal/current-theme-dark)))
    (disable-theme minimal/current-theme) ;; Disable the current theme
    (load-theme new-theme t)             ;; Load the new theme
    (setq minimal/current-theme new-theme) ;; Update the variable holding the current theme
    (custom-set-variables `(minimal/current-theme ',new-theme)))) ;; Save the new theme setting

;; Keybinding to toggle the theme.
(global-set-key (kbd "C-c t") 'minimal/toggle-theme)

;; --- Frame Setup ---
;; Function to set up fonts and theme for new frames.
(defun minimal/setup-frame (frame)
  "Setup fonts, theme, and modeline for new frames."
  (with-selected-frame frame
    (load-theme minimal/current-theme t) ;; Load the current theme
    (set-frame-font minimal/default-font nil t))) ;; Set the default font

;; Apply frame setup when Emacs starts or when a new frame is created.
(if (daemonp)
    ;; If Emacs is running as a daemon, hook setup to new frames.
    (add-hook 'after-make-frame-functions #'minimal/setup-frame)
  ;; Otherwise, set up the current frame.
  (minimal/setup-frame (selected-frame)))

;; --- Custom File Loading ---
;; Path to the custom file.
(setq minimal/custom-file "~/.emacs.d/custom.el")

;; Create the custom file if it doesn't exist.
(if (not (file-exists-p minimal/custom-file))
    (make-empty-file minimal/custom-file))

;; Load custom file if it exists.
(when (file-exists-p minimal/custom-file)
  (setq custom-file minimal/custom-file) ;; Set Emacs's custom-file variable
  (load-file custom-file))             ;; Load the custom file
