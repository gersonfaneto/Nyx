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
  (let ((eglot-ensure-restart 'ignore))
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

;; --- General Keybindings ---
(global-set-key (kbd "C-c C-c r") 'restart-emacs)
(global-set-key (kbd "C-c C-c k") 'kill-emacs)


;; --- Lisp Keybindings
(global-set-key (kbd "C-x C-j") 'eval-print-last-sexp)

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
(use-package straight :straight (:type built-in)
  :custom
  (straight-use-package-by-default t)) ;; Ensure all packages are managed by straight.el

;; --- Server Mode ---
;; Start Emacs server if not already running, for emacsclient.
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

;; --- Themes and Appearance ---
(use-package solarized-theme
  :config
  (progn
    (defun minimal/toggle-solarized-background ()
      "Switch between dark/light modes of the Solarized color theme."
      (interactive)
      (setq frame-background-mode
            (if (eq frame-background-mode 'dark) 'light 'dark))
      (load-theme
       (intern
        (format "solarized-%s" frame-background-mode)))
    (mapc 'frame-set-background-mode (frame-list)))
  (global-set-key (kbd "C-M-0") 'minimal/toggle-solarized-background)))

;; Install all-the-icons if in graphical mode.
(use-package all-the-icons
  :if
  (display-graphic-p) ;; Only load if Emacs has a graphical interface
  :commands
  all-the-icons-install-fonts
  :config
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t))) ;; Install icons if not found

(use-package nano-modeline
  :init
  (setq nano-modeline-position 'nano-modeline-footer)
  :config
  (progn
    (defun minimal/nano-modeline-update (&rest _)
      "Update nano-modeline active face."
      (custom-set-faces
       `(nano-modeline-active
	 ((t (:foreground ,(face-foreground 'default)
			  :background ,(face-background 'header-line nil t)
			  :box (:line-width 1 :color ,(face-background 'default))))))))
    (add-hook 'enable-theme-functions #'minimal/nano-modeline-update))
  :hook
  ((prog-mode-hook . nano-modeline-prog-mode)
   (text-mode-hook . nano-modeline-text-mode)
   (org-mode-hook . nano-modeline-org-mode)
   (pdf-view-mode-hook . nano-modeline-pdf-mode)
   (mu4e-headers-mode-hook . nano-modeline-mu4e-headers-mode)
   (mu4e-view-mode-hook  . nano-modeline-mu4e-message-mode)
   (elfeed-show-mode-hook . nano-modeline-elfeed-entry-mode)
   (elfeed-search-mode-hook . nano-modeline-elfeed-search-mode)
   (term-mode-hook . nano-modeline-term-mode)
   (xwidget-webkit-mode-hook . nano-modeline-xwidget-mode)
   (messages-buffer-mode-hook . nano-modeline-message-mode)
   (org-capture-mode-hook . nano-modeline-org-capture-mode)
   (org-agenda-mode-hook . nano-modeline-org-agenda-mode)))

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
	("?" . dirvish-dispatch)
	("-" . dired-up-directory))
  :bind
  ("M--" . dired-jump)
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
  ("C-c m s" . 'magit-status)
  ("C-c m l" . 'magit-log))

;; --- Environment ----
;; Direnv for managing environment variables in projects.
(use-package direnv
  :config
  (direnv-mode))                      ;; Enable direnv mode

;; --- Terminal ---
;; Shell with a nearly universal compatibility with terminal applications.
(use-package vterm
  :bind
  (("s-v" . vterm-yank)
   ("M-y" . vterm-yank)))

(use-package vterm-toggle
  :bind*
  ("C-t" . vterm-toggle)) ; "Intelligent" switching to vterm; eg creates it if it's not open, non-intrusive windowing, saves window setup, etc.

;; --- Popper ---
(use-package popper
  :bind
  (("C-`"   . popper-toggle)
   ("M-`"   . popper-cycle)
   ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode 1)
  (popper-echo-mode 1))

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

;; --- Eglot ---
(use-package eglot :straight (:type built-in)
  :hook (eglot-managed-mode
         . (lambda ()
             (add-hook 'flymake-diagnostic-functions 'eglot-flymake-backend)
             (setq indent-region-function 'eglot-format)))
  :bind (:map eglot-mode-map
              ("C-c l r" . eglot-rename)
              ("C-c l f" . eglot-format-buffer)
	      ("C-c l a" . eglot-code-actions)
	      ("C-c l d" . eglot-code-find-declaration)
	      ("C-c l D" . eglot-code-find-typeDefinition)
	      ("C-c l i" . eglot-code-find-implementation))
  :bind
  ("C-c e r" . eglot-reconnect)
  ("C-c e s" . eglot-shutdown)
  ("C-c e S" . eglot-shutdown-all)
  :config
  (setq-default eglot-stay-out-of '(flymake-diagnostic-functions
                                    eldoc-documentation-strategy)))

;; --- Lisp ---
(use-package paredit
  :defer t
  :bind
  (:map paredit-mode-map ("RET" . nil))
  :hook
  ((cider-repl-mode
    clojure-mode
    emacs-lisp-mode
    geiser-repl-mode
    ielm-mode
    lisp-interaction-mode
    lisp-mode
    racket-mode
    racket-repl-mode
    scheme-mode
    slime-repl-mode)
   . paredit-mode))

;; (use-package paredit
;;   :ensure t
;;   :config
;;   (dolist (m '(emacs-lisp-mode-hook
;;                racket-mode-hook
;;                racket-repl-mode-hook))
;;     (add-hook m #'paredit-mode))
;;   (bind-keys :map paredit-mode-map
;;              ("{"   . paredit-open-curly)
;;              ("}"   . paredit-close-curly))
;;   (unless terminal-frame
;;     (bind-keys :map paredit-mode-map
;;                ("M-[" . paredit-wrap-square)
;;                ("M-{" . paredit-wrap-curly))))

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
  "\\.nix\\'"
  :hook
  (nix-mode . eglot-ensure)
  :init
  (setq lsp-nix-nixd-server-path "nixd"
	lsp-nix-nixd-formatting-command [ "alejandra" ]
	lsp-nix-nixd-nixpkgs-expr "import <nixpkgs> { }"
	lsp-nix-nixd-nixos-options-expr "(builtins.getFlake (\"git+file://\" + toString ./.)).nixosConfigurations.Nyx.options"
	lsp-nix-nixd-home-manager-options-expr "(builtins.getFlake (\"git+file://\" + toString ./.)).homeConfigurations.\"gerson@Nyx\".options")
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
		 '((nix-mode) . ("nixd" :initializationOptions
				 (:formatting (:command ["alejandra"])))))))

;; --- C/C++ Mode ---
(use-package cc-mode :straight (:type built-in)
  :hook
  (c-mode . eglot-ensure))

;; --- Rust Mode ---
(use-package rust-mode
  :ensure t
  :hook (rust-mode . eglot-ensure))

;; --- Lua Mode ---
(use-package lua-mode
  :hook
  (lua-mode . eglot-ensure))

;; --- Player Mode ---
(use-package ready-player
  :init
  (ready-player-mode 1)
  :bind
  ("C-c m d" . ready-player-download-album-artwork-and-set-metadata)
  :custom
  (ready-player-my-media-collection-location "~/Music/"))

;; --- UI :: Theme ---
;; Advice to clear previous themes before loading a new one.
(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them."
  (mapc #'disable-theme custom-enabled-themes))

;; Allow safe custom themes.
(setq custom-safe-themes t)

;; Load default theme with default background.
(progn
  (defun minimal/load-theme ()
    (load-theme
     (intern
      (format "%s-%s" minimal/default-theme minimal/default-background))))
  (minimal/load-theme))

;; Set the frame default background.
(progn
  (setq frame-background-mode minimal/default-background)
  (mapc 'frame-set-background-mode (frame-list)))

;; --- UI :: Transparency ---
;; Change values of frame alpha to toggle it between solid and seetrough.
(defun minimal/toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (if (eq
     (if (numberp alpha)
	 alpha
       (cdr alpha))
     100)
    (set-frame-parameter nil 'alpha '(90 . 50))
      (set-frame-parameter nil 'alpha '(100 . 100)))))

;; FIXME: Doesn't work on the first run...
(progn
  (minimal/toggle-transparency))

;; Keybinding to toggle the transparency.
(global-set-key (kbd "C-M-9") 'minimal/toggle-transparency)

;; --- Frame Setup ---
;; Function to set up fonts and theme for new frames.
(defun minimal/setup-frame (frame)
  "Setup fonts, theme, and modeline for new frames."
  (with-selected-frame frame
    (minimal/load-theme)				;; Load the current theme
    (set-frame-font minimal/default-font nil t)))	;; Set the default font

;; Apply frame setup when Emacs starts or when a new frame is created.
(if (daemonp)
    ;; If Emacs is running as a daemon, hook setup to new frames.
    (add-hook 'after-make-frame-functions #'minimal/setup-frame)
  ;; Otherwise, set up the current frame.
  (minimal/setup-frame (selected-frame)))
