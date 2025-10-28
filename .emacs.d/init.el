(tool-bar-mode   0)
(menu-bar-mode   0)
(scroll-bar-mode 0)

(setq inhibit-startup-screen t
      initial-scratch-message "")

(setq tab-width 2
      fill-column 100
      indent-tabs-mode nil)

(setq backup-directory-alist `(("." . "~/.emacs.d/backup/"))
      auto-save-file-name-transforms `((".*" "~/.emacs.d/backup/" t)))

(setq use-short-answers t
      compilation-ask-about-save nil
      confirm-nonexistent-file-or-buffer nil)

(setq dired-listing-switches "-lhAX --group-directories-first")

(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

(set-frame-font "Nova Nerd Font 12" nil t)

(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "M-o") 'other-window)

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

(global-set-key (kbd "C-x C-p") 'previous-buffer)
(global-set-key (kbd "C-x C-n") 'next-buffer)
(global-set-key (kbd "C-x C-k") 'kill-current-buffer)

(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c r") 'recompile)

(defvar minimal/current-theme-light 'modus-operandi
  "Light variant of current theme.")

(defvar minimal/current-theme-dark 'modus-vivendi
  "Dark variant of current theme.")

(defvar minimal/current-theme minimal/current-theme-dark
  "Current theme being used.")

(defun minimal/setup-frame (frame)
  "Setup fonts, theme, and modeline for new frames."
  (with-selected-frame frame
    (load-theme minimal/current-theme t)
    (set-frame-font "Nova Nerd Font 12" nil t)))

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

(if (daemonp)
    (add-hook 'after-make-frame-functions #'minimal/setup-frame)
  (minimal/setup-frame (selected-frame)))

(setq minimal/custom-file "~/.emacs.d/custom.el")

(if (not (file-exists-p minimal/custom-file))
    (make-empty-file minimal/custom-file))

(when (file-exists-p minimal/custom-file)
  (setq custom-file minimal/custom-file)
  (load-file custom-file))
