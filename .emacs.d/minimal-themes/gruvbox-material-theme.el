;;; gruvbox-material-theme.el --- Gruvbox Material theme -*- lexical-binding: t; -*-

(deftheme gruvbox-material
  "Gruvbox Material theme with dark/light variants, contrast levels,
true-color detection, and 256-color fallback.")

;;; Options

(defgroup gruvbox-material nil
  "Gruvbox Material theme options."
  :group 'faces)

(defcustom gruvbox-material-style 'dark
  "Theme style: 'dark or 'light."
  :type '(choice (const dark) (const light)))

(defcustom gruvbox-material-contrast 'medium
  "Contrast level: 'hard, 'medium, or 'soft."
  :type '(choice (const hard) (const medium) (const soft)))

;;; True color detection

(defun gruvbox-material--truecolor-p ()
  "Return non-nil if Emacs supports true color."
  (or (display-graphic-p)
      (string-match-p "truecolor\\|24bit"
                      (or (getenv "COLORTERM") ""))))

;;; Palettes

(defun gruvbox-material--palette-truecolor ()
  "Return the true-color palette based on style and contrast."
  (pcase (list gruvbox-material-style gruvbox-material-contrast)
    (`(dark hard)
     '((bg0 . "#1d2021") (bg1 . "#282828") (bg2 . "#32302f")
       (fg0 . "#fbf1c7") (fg1 . "#ebdbb2") (fg2 . "#d5c4a1")
       (red . "#fb4934") (green . "#b8bb26") (yellow . "#fabd2f")
       (blue . "#83a598") (purple . "#d3869b")
       (aqua . "#8ec07c") (orange . "#fe8019")
       (gray . "#928374")))
    (`(dark soft)
     '((bg0 . "#32302f") (bg1 . "#3c3836") (bg2 . "#504945")
       (fg0 . "#ebdbb2") (fg1 . "#d5c4a1") (fg2 . "#bdae93")
       (red . "#fb4934") (green . "#b8bb26") (yellow . "#fabd2f")
       (blue . "#83a598") (purple . "#d3869b")
       (aqua . "#8ec07c") (orange . "#fe8019")
       (gray . "#928374")))
    (`(light hard)
     '((bg0 . "#f9f5d7") (bg1 . "#ebdbb2") (bg2 . "#d5c4a1")
       (fg0 . "#282828") (fg1 . "#3c3836") (fg2 . "#504945")
       (red . "#cc241d") (green . "#98971a") (yellow . "#d79921")
       (blue . "#458588") (purple . "#b16286")
       (aqua . "#689d6a") (orange . "#d65d0e")
       (gray . "#7c6f64")))
    (_ ;; medium (dark or light default)
     '((bg0 . "#282828") (bg1 . "#3c3836") (bg2 . "#504945")
       (fg0 . "#fbf1c7") (fg1 . "#ebdbb2") (fg2 . "#d5c4a1")
       (red . "#fb4934") (green . "#b8bb26") (yellow . "#fabd2f")
       (blue . "#83a598") (purple . "#d3869b")
       (aqua . "#8ec07c") (orange . "#fe8019")
       (gray . "#928374")))))

(defconst gruvbox-material--palette-256
  '((bg0 . "color-235")
    (bg1 . "color-237")
    (bg2 . "color-239")
    (fg0 . "color-230")
    (fg1 . "color-223")
    (fg2 . "color-250")
    (red . "color-167")
    (green . "color-142")
    (yellow . "color-214")
    (blue . "color-109")
    (purple . "color-175")
    (aqua . "color-108")
    (orange . "color-208")
    (gray . "color-245")))

(defun gruvbox-material--color (key palette)
  "Resolve KEY from PALETTE or 256-color fallback."
  (if (gruvbox-material--truecolor-p)
      (cdr (assoc key palette))
    (cdr (assoc key gruvbox-material--palette-256))))

;;; Faces

(let* ((palette (gruvbox-material--palette-truecolor))
       (bg0 (gruvbox-material--color 'bg0 palette))
       (bg1 (gruvbox-material--color 'bg1 palette))
       (bg2 (gruvbox-material--color 'bg2 palette))
       (fg0 (gruvbox-material--color 'fg0 palette))
       (fg1 (gruvbox-material--color 'fg1 palette))
       (fg2 (gruvbox-material--color 'fg2 palette))
       (red (gruvbox-material--color 'red palette))
       (green (gruvbox-material--color 'green palette))
       (yellow (gruvbox-material--color 'yellow palette))
       (blue (gruvbox-material--color 'blue palette))
       (purple (gruvbox-material--color 'purple palette))
       (aqua (gruvbox-material--color 'aqua palette))
       (orange (gruvbox-material--color 'orange palette))
       (gray (gruvbox-material--color 'gray palette)))

  (custom-theme-set-faces
   'gruvbox-material

   ;; Core UI
   `(default ((t (:background ,bg0 :foreground ,fg1))))
   `(cursor ((t (:background ,fg1))))
   `(fringe ((t (:background ,bg0))))
   `(region ((t (:background ,bg2))))
   `(highlight ((t (:background ,bg2))))
   `(shadow ((t (:foreground ,gray))))
   `(link ((t (:foreground ,blue :underline t))))

   `(mode-line ((t (:background ,bg1 :foreground ,fg1))))
   `(mode-line-inactive ((t (:background ,bg0 :foreground ,gray))))

   ;; Line numbers
   `(line-number ((t (:background ,bg0 :foreground ,gray))))
   `(line-number-current-line ((t (:background ,bg1 :foreground ,fg1))))

   ;; Syntax
   `(font-lock-keyword-face ((t (:foreground ,red :weight bold))))
   `(font-lock-function-name-face ((t (:foreground ,green))))
   `(font-lock-type-face ((t (:foreground ,yellow))))
   `(font-lock-constant-face ((t (:foreground ,purple))))
   `(font-lock-variable-name-face ((t (:foreground ,fg1))))
   `(font-lock-string-face ((t (:foreground ,green))))
   `(font-lock-comment-face ((t (:foreground ,gray :slant italic))))
   `(font-lock-builtin-face ((t (:foreground ,orange))))
   `(font-lock-preprocessor-face ((t (:foreground ,orange))))

   ;; Tree-sitter (Emacs 29+)
   `(treesit-face-keyword ((t (:foreground ,red :weight bold))))
   `(treesit-face-function ((t (:foreground ,green))))
   `(treesit-face-type ((t (:foreground ,yellow))))
   `(treesit-face-variable ((t (:foreground ,fg1))))
   `(treesit-face-constant ((t (:foreground ,purple))))
   `(treesit-face-property ((t (:foreground ,blue))))
   `(treesit-face-comment ((t (:foreground ,gray :slant italic))))

   ;; LSP
   `(lsp-face-highlight-textual ((t (:background ,bg2))))
   `(lsp-face-highlight-read ((t (:background ,bg2))))
   `(lsp-face-highlight-write ((t (:background ,bg2))))
   `(lsp-ui-doc-background ((t (:background ,bg1))))
   `(lsp-ui-peek-header ((t (:background ,bg2 :foreground ,yellow))))
   `(lsp-ui-peek-selection ((t (:background ,bg2 :weight bold))))

   ;; Company
   `(company-tooltip ((t (:background ,bg1 :foreground ,fg1))))
   `(company-tooltip-selection ((t (:background ,bg2))))
   `(company-tooltip-common ((t (:foreground ,yellow))))
   `(company-scrollbar-bg ((t (:background ,bg2))))
   `(company-scrollbar-fg ((t (:background ,bg1))))

   ;; Ivy / Vertico
   `(ivy-current-match ((t (:background ,bg2 :weight bold))))
   `(ivy-minibuffer-match-face-1 ((t (:foreground ,yellow))))
   `(vertico-current ((t (:background ,bg2))))

   ;; Magit
   `(magit-section-heading ((t (:foreground ,yellow :weight bold))))
   `(magit-branch-local ((t (:foreground ,green))))
   `(magit-branch-remote ((t (:foreground ,aqua))))
   `(magit-diff-added ((t (:foreground ,green))))
   `(magit-diff-removed ((t (:foreground ,red))))
   `(magit-diff-context ((t (:foreground ,gray))))

   ;; Org
   `(org-document-title ((t (:foreground ,yellow :weight bold :height 1.4))))
   `(org-level-1 ((t (:foreground ,red :weight bold :height 1.2))))
   `(org-level-2 ((t (:foreground ,orange :weight bold :height 1.15))))
   `(org-level-3 ((t (:foreground ,yellow :weight bold))))
   `(org-level-4 ((t (:foreground ,green))))
   `(org-level-5 ((t (:foreground ,aqua))))
   `(org-block ((t (:background ,bg1 :extend t))))
   `(org-block-begin-line ((t (:foreground ,gray :background ,bg1))))
   `(org-block-end-line ((t (:foreground ,gray :background ,bg1))))
   `(org-code ((t (:foreground ,aqua))))
   `(org-verbatim ((t (:foreground ,fg2))))
   `(org-quote ((t (:slant italic :background ,bg1))))
   `(org-table ((t (:foreground ,blue))))
   `(org-checkbox ((t (:foreground ,green :weight bold))))
   `(org-todo ((t (:foreground ,red :weight bold))))
   `(org-done ((t (:foreground ,green :weight bold))))
   `(org-date ((t (:foreground ,blue))))
   `(org-link ((t (:foreground ,blue :underline t))))

   ;; LaTeX
   `(font-latex-sectioning-5-face ((t (:foreground ,yellow :weight bold))))
   `(font-latex-string-face ((t (:foreground ,green))))
   `(font-latex-keyword-face ((t (:foreground ,red))))
   `(font-latex-math-face ((t (:foreground ,blue))))
   `(font-latex-script-char-face ((t (:foreground ,purple))))
   `(font-latex-warning-face ((t (:foreground ,orange :weight bold))))))

(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory
                (file-name-directory load-file-name))))

(provide-theme 'gruvbox-material)
