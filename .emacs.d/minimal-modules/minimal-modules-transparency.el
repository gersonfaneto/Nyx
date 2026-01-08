;; -*- lexical-binding: t; -*-
;;
;; Minimal DWIM transparency support
;; Author: gersonfaneto
;;
;; Responsibilities:
;; - Read global transparency state (shell-controlled)
;; - Apply transparency deterministically to frames
;; - Provide a DWIM interactive command
;; - Be safe for daemon + after-make-frame-functions

;;; Code:

;; -------------------------------------------------------------------
;; Configuration
;; -------------------------------------------------------------------

(defconst minimal/transparency-state-file
  (expand-file-name
   "ui/transparency"
   (or (getenv "XDG_STATE_HOME")
       (expand-file-name "~/.local/state")))
  "Path to the global transparency state file.")

(defconst minimal/transparency-alpha-transparent 90
  "Alpha value used when transparency is enabled.")

(defconst minimal/transparency-alpha-opaque 100
  "Alpha value used when transparency is disabled.")

;; -------------------------------------------------------------------
;; State helpers
;; -------------------------------------------------------------------

(defun minimal/transparency-enabled-p ()
  "Return non-nil if global transparency state is ON."
  (when (file-readable-p minimal/transparency-state-file)
    (string=
     (string-trim
      (with-temp-buffer
	(insert-file-contents minimal/transparency-state-file)
	(buffer-string)))
     "on")))

(defun minimal/current-alpha (&optional frame)
  "Return current alpha-background of FRAME (default: selected frame)."
  (or (frame-parameter frame 'alpha-background)
      minimal/transparency-alpha-opaque))

;; -------------------------------------------------------------------
;; Pure setters (NO toggling)
;; -------------------------------------------------------------------

(defun minimal/set-transparency (alpha &optional frame)
  "Set FRAME transparency to ALPHA."
  (set-frame-parameter (or frame (selected-frame))
		       'alpha-background alpha))

(defun minimal/apply-transparency (&optional frame)
  "Apply global transparency state to FRAME."
  (minimal/set-transparency
   (if (minimal/transparency-enabled-p)
       minimal/transparency-alpha-transparent
     minimal/transparency-alpha-opaque)
   frame))

;; -------------------------------------------------------------------
;; Local (frame-only) toggle
;; -------------------------------------------------------------------

(defun minimal/toggle-frame-transparency (&optional frame)
  "Toggle transparency for FRAME only."
  (let* ((frame (or frame (selected-frame)))
	 (current (minimal/current-alpha frame)))
    (minimal/set-transparency
     (if (= current minimal/transparency-alpha-opaque)
	 minimal/transparency-alpha-transparent
       minimal/transparency-alpha-opaque)
     frame)))

;; -------------------------------------------------------------------
;; DWIM command
;; -------------------------------------------------------------------

(defun minimal/transparency-dwim (arg)
  "Do-What-I-Mean transparency command.

No prefix:
  Toggle transparency for the current frame only.

C-u:
  Apply global transparency state.

C-u C-u:
  Force opaque.

C-u C-u C-u:
  Force transparent."
  (interactive "P")
  (pcase arg
    ('nil
     (minimal/toggle-frame-transparency))
    ('(4)
     (minimal/apply-transparency))
    ('(16)
     (minimal/set-transparency minimal/transparency-alpha-opaque))
    ('(64)
     (minimal/set-transparency minimal/transparency-alpha-transparent))
    (_
     (minimal/toggle-frame-transparency))))

;; -------------------------------------------------------------------
;; Frame integration
;; -------------------------------------------------------------------

(defun minimal/setup-transparency-for-frame (frame)
  "Apply correct transparency to newly created FRAME."
  (with-selected-frame frame
    (minimal/apply-transparency frame)))

(if (daemonp)
    (add-hook 'after-make-frame-functions
	      #'minimal/setup-transparency-for-frame)
  (minimal/setup-transparency-for-frame (selected-frame)))

;; -------------------------------------------------------------------
;; Keybinding
;; -------------------------------------------------------------------

(global-set-key (kbd "C-M-9") #'minimal/transparency-dwim)

(provide 'minimal-modules-transparency)

;;; minimal-transparency.el ends here
