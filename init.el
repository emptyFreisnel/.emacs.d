;;; package --- init.el -*- lexical-binding: t -*-

;;; Commentary:
;;  emptyFreisnel's personal Emacs config...

;; Setup package manager...using Elpaca.
;; Do remember to update the packages using elpaca-update.

;;; Code:
(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
			      :ref nil :depth 1 :inherit ignore
			      :files (:defaults "elpaca-test.el" (:exclude "extensions"))
			      :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
	(if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
		  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
						  ,@(when-let* ((depth (plist-get order :depth)))
						      (list (format "--depth=%d" depth) "--no-single-branch"))
						  ,(plist-get order :repo) ,repo))))
		  ((zerop (call-process "git" nil buffer t "checkout"
					(or (plist-get order :ref) "--"))))
		  (emacs (concat invocation-directory invocation-name))
		  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
					"--eval" "(byte-recompile-directory \".\" 0 'force)")))
		  ((require 'elpaca))
		  ((elpaca-generate-autoloads "elpaca" repo)))
	    (progn (message "%s" (buffer-string)) (kill-buffer buffer))
	  (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;; ============================================================================
;;  Auto garbage collection when emacs loses focus and using the minibuffer.
;;  See: https://github.com/MatthewZMD/.emacs.d/blob/master/init.el
;; ============================================================================

(defvar init-gc-cons-threshold 134217728 ;; 128MB
  "Default value to use for `gc-cons-threshold'.
If you experience freezing, decrease this.
If you experience stuttering, increase this.")

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (if (boundp 'after-focus-change-function)
		(add-function :after after-focus-change-function
			      (lambda ()
				(unless (frame-focus-state)
				  (garbage-collect))))
	      (add-hook 'after-focus-change-function 'garbage-collect))
	    (defun gc-minibuffer-setup-hook ()
	      (setq gc-cons-threshold (* init-gc-cons-threshold 2)))
	    (defun gc-minibuffer-exit-hook ()
	      (garbage-collect)
	      (setq gc-cons-threshold init-gc-cons-threshold))

	    (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
	    (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))

;; ============================================================================
;;  Setting up use-package
;; ============================================================================

(use-package use-package
  :ensure nil
  :custom
  (use-package-compute-statistics t))

;; ============================================================================
;;  Compile-angel to make sure packages are natively compiled.
;;  this package actually makes my emacs initialise slower...
;;  so i only activate it when there is a new package(s).
;; ============================================================================

(use-package compile-angel
  :ensure t
  :commands compile-angel-on-load-mode
  ;; :demand t
  :config
  (setq compile-angel-verbose t) ;; set to nil to silence compile-angel.
  (push "init.el" compile-angel-excluded-files)
  (push "early-init.el" compile-angel-excluded-files)
  (push "Angelique!.el" compile-angel-excluded-files)
  (push "~/.emacs.d/var/recentf-save.el" compile-angel-excluded-files)
  (compile-angel-on-load-mode)
  (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode))

;; ============================================================================
;;  A clean emacs is a happy emacs! which makes ya girl happy!
;; ============================================================================

(use-package no-littering
  :ensure t)

;; ============================================================================
;;  Aesthetics (themes, icons, fonts and bars)...important!!!
;; ============================================================================

(use-package nerd-icons
  :ensure t)
   
(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :init
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package doom-themes
  :ensure t
  :commands doom-themes-visual-bell-config
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :init
  (load-theme 'doom-outrun-electric t)
  (doom-themes-visual-bell-config))

(use-package doom-modeline
  :ensure t
  :hook (elpaca-after-init . doom-modeline-mode))

(use-package indent-bars
  :ensure t
  :defer 1
  :hook ((prog-mode) . indent-bars-mode))

(use-package rainbow-delimiters
  :ensure t
  :defer 1
  :hook ((prog-mode) . rainbow-delimiters-mode))

(use-package colorful-mode
  :ensure t
  :defer 1
  :hook ((prog-mode
	  org-mode) . colorful-mode))

(use-package info-colors
  :ensure t
  :config
  (add-hook 'Info-selection-hook 'info-colors-fontify-node))

;; ============================================================================
;;  Ibuffer customizations.
;; ============================================================================

(use-package ibuffer
  :ensure nil
  :custom
  (ibuffer-human-readable-size t)
  (ibuffer-always-show-last-buffer t)
  (ibuffer-truncate-lines nil)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-display-summary nil)
  (ibuffer-expert t)
  (ibuffer-title-face '(:inherit (font-lock-doc-markup-face)))
  (ibuffer-filter-group-name-face '(:inherit (font-lock-doc-face bold)))
  :bind
  (("C-x b" . ibuffer)
   ("C-x C-b" . ibuffer-other-window)
   :map ibuffer-mode-map
   ("M-o" . nil))
  :hook (ibuffer-mode . ibuffer-auto-mode))

(use-package ibuffer-project
  :ensure t
  :custom
  (ibuffer-project-use-cache t)
  ;; Lovingly taken and amended from
  ;; https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-ibuffer.el
  (ibuffer-project-root-functions
   `((ibuffer-project-project-root
      . ,(concat (nerd-icons-octicon "nf-oct-repo"
				    :height 1.2
				    :face ibuffer-filter-group-name-face)
		 " Project"))
     (identity
      . ,(concat (nerd-icons-octicon "nf-oct-file_directory"
				    :height 1.2
				    :face ibuffer-filter-group-name-face)
		 " Directory"))
     (file-remote-p
      . ,(concat (nerd-icons-codicon "nf-cod-radio_tower"
				    :height 1.2
				    :face ibuffer-filter-group-name-face)
		 " Remote"))))
  :config
  (defun ibuffer-project-function ()
    "Group ibuffer's list by project."
    (interactive)
    (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
    (unless (eq ibuffer-sorting-mode 'project-file-relative)
      (ibuffer-do-sort-by-project-file-relative)))
  :hook (ibuffer . ibuffer-project-function))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;; ============================================================================
;;  Dashboard...because emacs needs to be cute!!!
;;  Need to see about enlight...but maybe create my own someday?
;; ============================================================================

(use-package dashboard
  :ensure t
  :custom
  (dashboard-banner-logo-title
   "˚₊‧꒰ა 🎀 ໒꒱ ‧₊˚ Welcome, emptyFresinel, my beloved Hacker Princess! ˚₊‧꒰ა 🎀 ໒꒱ ‧₊˚")
  (dashboard-startup-banner
   (list '(("~/.emacs.d/Angelique!/Pictures/puroseka/Ena_21_trained_art.png(r).png")
	   ("~/.emacs.d/Angelique!/Pictures/puroseka/Ena_39_trained_art(r).png")
	   ("~/.emacs.d/Angelique!/Pictures/puroseka/Mizuki_3_art.png(r).png")
	   ("~/.emacs.d/Angelique!/Pictures/puroseka/Mizuki_7_trained_art.png(r).png")
	   ("~/.emacs.d/Angelique!/Pictures/puroseka/Mizuki_8_trained_art.png(r).png")
	   ("~/.emacs.d/Angelique!/Pictures/puroseka/Mizuki_20_trained_art.png(r).png")
	   ("~/.emacs.d/Angelique!/Pictures/puroseka/Mizuki_38_trained_art(r).png")
	   ("~/.emacs.d/Angelique!/Pictures/puroseka/Kanade_41_art(r).png")
	   ("~/.emacs.d/Angelique!/Pictures/puroseka/Kanade_21_trained_art(r).png")
	   ("~/.emacs.d/Angelique!/Pictures/puroseka/Kanade_28_trained_art(r).png")
	   ("~/.emacs.d/Angelique!/Pictures/puroseka/Mafuyu_20_trained_art.png(r).png"))))

  (dashboard-projects-backend 'project-el)
  (dashboard-center-content t)

  ;; Use project-known-project-roots to check known projects.
  ;; Also use project-forget-project to remove dead/deleted projects.
  ;; project-remember-projects-under is useful to add new projects too

  (dashboard-items '((recents . 5)
		     (projects . 5)))

  (dashboard-item-shortcuts '((recents . "r")
			      (projects . "p")))
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-remove-missing-entry)
  :config
  (set-face-attribute 'dashboard-banner-logo-title nil
		      :family "VictorMono Nerd Font Mono"
		      :slant 'italic
		      :foreground "#DF85FF"
		      :height 120)
  (set-face-attribute 'dashboard-items-face nil
		      :foreground "#A875FF")
  (setq initial-buffer-choice (lambda ()
				(get-buffer-create dashboard-buffer-name)))
  (dashboard-setup-startup-hook)
  :hook
  (elpaca-after-init . dashboard-insert-startupify-lists)
  (elpaca-after-init . dashboard-initialize))

;; ============================================================================
;;  Wish emacs can expose more properties to font attributes
;;  or let us set priorities for layering. As it stands the region is
;;  an overlay so it has higher priority over text properties. so setting
;;  something to foreground overrides font-lock syntax highlighting...
;; ============================================================================

;; using daemon-mode...use default-frame-alist instead of
;; set-frame-font. also also, use menu-set-font to check out new fonts.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Fonts.html

;; (add-to-list 'default-frame-alist '(font . "JetBrains Mono Nerd Font 12"))
;; (add-to-list 'default-frame-alist '(font . "CaskaydiaMono Nerd Font Mono 12"))

(add-to-list 'default-frame-alist '(font . "FiraCode Nerd Font Propo 11"))
(add-to-list 'default-frame-alist '(cursor-color ."cyan"))

(add-to-list 'default-frame-alist '(height . 45))
(add-to-list 'default-frame-alist '(width . 120))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(defun Angelique!--current-frame-customisations (&optional frame)

  "For various aesthetic customisations to be loaded in FRAME.
This will hopefully make sure that emacsclient also
inherit the customisations properly."

  (with-selected-frame (or frame (selected-frame))
    (set-face-attribute 'region nil
			:background "#4B0082") ;; indigo

    (set-face-attribute 'highlight nil
			:background "#EEAEEE")

    (set-face-attribute 'lazy-highlight nil
			:background "#EEAEEE")

    (set-face-attribute 'isearch nil
			:background "#EEAEEE")

    (set-face-attribute 'hl-line nil
			:background "#120333")

    (set-face-attribute 'line-number-current-line nil
			:foreground "cyan")))

(add-hook 'elpaca-after-init-hook #'Angelique!--current-frame-customisations)
(if (daemonp) (add-hook 'server-after-make-frame-hook
			#'Angelique!--current-frame-customisations))

;; ============================================================================
;;  Setting up Angelique! folder for my own custom
;;  functions and perhaps my own little packages.
;;  Some customisations that did not thematically fit go here for now.
;; ============================================================================

;; not using customize feature.
(setq custom-file null-device)

;; easier pop from C-u C-SPC
(setq set-mark-command-repeat-pop t)

;; shift-select-mode set to permanent
(setq shift-select-mode 'permanent)

;; No need for ~ files when editing
(setq-default create-lockfiles nil)

(require 'server)
(unless (server-running-p)
  (server-start))

(defvar Angelique! (expand-file-name "Angelique!/" user-emacs-directory))
(defvar 庭園の王女 (expand-file-name "庭園の王女/" Angelique!))

(unless (file-directory-p Angelique!)
  (make-directory Angelique! t))
(unless (file-directory-p 庭園の王女)
  (make-directory 庭園の王女 t))

;; ============================================================================
;;  Home row keybindings...and packages that assist in moving sentences.
;; ============================================================================

(use-package transient
  :ensure (:wait t) ;; otherwise load before elpaca.
  :config
  (require 'transient))

;; See karthik's post on how to use avy.
(use-package avy
  :ensure t
  :defer 2
  :commands (avy-goto-char-2
	     avy-goto-char-timer)
  :config
  (defun Angelique!--avy-action-helpful (pt)
    (save-excursion
      (goto-char pt)
      (helpful-at-point))
    (select-window (cdr (ring-ref avy-ring 0))) t)
  
  (transient-define-prefix Angelique!--avy-commands ()
    "Transient inteface for avy commands."
    ["Angelique! avy-commands..."
     ["Goto"
      ("w" "avy-goto-char"
       (lambda () (interactive) (call-interactively #'avy-goto-char)))
      ("r" "avy-goto-word-1"
       (lambda () (interactive) (call-interactively #'avy-goto-word-1)))
      ("t" "avy-goto-char-timer"
       (lambda () (interactive) (call-interactively #'avy-goto-char-timer)))
      ("l" "avy-goto-line"
       (lambda () (interactive) (call-interactively #'avy-goto-line)))]])
 
  :custom
  (avy-keys '(?c ?r ?s ?t ?b ?f ?n ?e ?i))
  (avy-timeout-seconds 0.35)
  (avy-dispatch-alist '((?q . avy-action-kill-move)
			(?Q . avy-action-kill-stay)
			(?w . avy-action-teleport)
			(?W . avy-action-mark)
			(?k . avy-action-copy)
			(?y . avy-action-yank)
			(?Y . avy-action-yank-line)
			(?z . avy-action-zap-to-char)
			(?h . Angelique!--avy-action-helpful)))
  :bind
  ("M-r" . Angelique!--avy-commands))

(use-package crux
  :ensure t
  :defer 2
  :bind
  ("C-a" . crux-move-beginning-of-line)
  ("C-k" . crux-smart-kill-line))

(use-package hydra
  :ensure t
  :config

  (defun Angelique!--normal-cursor ()
    "Cursor indicator for Angelique!"
    (interactive)
    (setq-default cursor-type 'box)
    (set-cursor-color "cyan")
    (set-face-attribute 'line-number-current-line nil
			:foreground "cyan")
    (message (propertize
	      "🩵(💠ᴗ͈ˬᴗ͈)🩵* Angelique! navigation deactivated! 🩵(💠ᴗ͈ˬᴗ͈)🩵*"
	      'face `(:foreground "cyan"))))

  (defun Angelique!--keybinds ()
    "~Dream the good dream, like a good pretty-princess should!♡(✿ᴗ͈ˬᴗ͈)♡*~."
    (interactive)
    (setq-default cursor-type 'bar)
    (set-cursor-color "#FF83FA")
    (set-face-attribute 'line-number-current-line nil
			:foreground "#FF83FA")
    (message (propertize
	      "🩷(🌸ᴗ͈ˬᴗ͈)🩷* Angelique! navigation activated! 🩷(🌸ᴗ͈ˬᴗ͈)🩷*"
	      'face `(:foreground "#FF83FA"))))

  (global-set-key (kbd "M-i")
   (defhydra Angelique!--hydra-map
     (:pre Angelique!--keybinds :post Angelique!--normal-cursor :color red)
     ;; Normal navigation.
     ("n" backward-char)
     ("e" next-line)
     ("i" previous-line)
     ("a" forward-char)
     ;; Slurping and barfing.
     ("c" sp-backward-slurp-sexp)
     ("t" sp-forward-slurp-sexp)
     ;; Advanced navigation.
     ("C-a" crux-move-beginning-of-line)
     ("C-e" move-end-of-line)
     ("M-r" avy-goto-char-2)
     ("M-o" backward-sentence)
     ("M-e" forward-sentence)
     ("M-a" right-word)
     ("M-n" left-word)
     ("M-u" sp-backward-symbol)
     ("M-y" sp-forward-symbol)
     ("C-M-n" backward-sexp)
     ("C-M-a" forward-sexp)
     ;; More easy to reach keys for yank/kill
     ("w" sp-delete-word)
     ("f" sp-kill-sexp)
     ("C-d" delete-char)
     ("C-w" kill-region)
     ("d" delete-region :color blue)
     ("p" delete-pair :color blue)
     ;; Misc
     ("M-i" nil "Quit"))))

(defun Angelique!--universal-compile ()
  "Invoke the `compile' command with prefix arg programmatically."
  (interactive)
  (let* ((current-prefix-arg '(4)))
    (call-interactively #'compile)))

(transient-define-prefix Angelique!--easy-commands ()
  "Transient layout for commands that has very frequent use."
  ["Angelique! easy-commands..."
   ["Shell & Compile"
    ("v" "vterm" vterm)
    ("V" "vterm-other-window" vterm-other-window)
    ("E" "eshell" eshell)
    ("c" "compile" compile)
    ("r" "recompile" recompile)
    ("a" "Angelique!--universal-compile" Angelique!--universal-compile)]
   ["Dogears.el"
    ("d" "dogears-remember" dogears-remember)
    ("g" "dogears-go" dogears-go)
    ("b" "dogears-back" dogears-back)
    ("l" "dogears-list" dogears-list)]
   ["winner-undo"
    ("u" "winner-undo" winner-undo :transient t)]
   ["eglot"
    ("e" "eglot-code-actions" eglot-code-actions)]
   ["tree-sitter"
    ("t" "treesit-explore" treesit-explore)]
   ["code-cells"
    ("C" "code-cells-eval" code-cells-eval)]
   ["Misc"
    ("C-u" "universal-argument" universal-argument)
    ("M" "view-echo-area-messages" view-echo-area-messages)
    ("s" "scratch-buffer" scratch-buffer)]])

(defun Angelique!--move-line-up ()
  "Move line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun Angelique!--move-line-down ()
  "Move line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(define-key global-map (kbd "C-c c") 'Angelique!--easy-commands)
(define-key global-map (kbd "M-<up>") 'Angelique!--move-line-up)
(define-key global-map (kbd "M-<down>") 'Angelique!--move-line-down)

;; additional defined keys here...
(define-key global-map (kbd "C-M-c") 'treesit-up-list)
(define-key global-map (kbd "C-M-d") 'treesit-down-list)

;; disabled commands go here...
(define-key global-map (kbd "C-x m") nil)
(define-key global-map (kbd "C-x n n") nil)

(with-eval-after-load 'comint
  (define-key comint-mode-map (kbd "M-f") #'comint-history-isearch-backward-regexp)
  (define-key comint-mode-map (kbd "M-r") nil))

;; ============================================================================
;;  Buffer toggle.
;;  https://www.reddit.com/r/emacs/comments/l4v1ux/one_of_the_most_useful_small_lisp_functions_in_my/
;; ============================================================================

(defun Angelique!--toggle-or-create (buffer-name buffer-create-fn &optional switch-cont)
  "Makes a toggle-function to have raise-or-create behaviour.

  Creates a toggle-function that executes BUFFER-CREATE-FN if a
  buffer named BUFFER-NAME doesn't exist, switches to the buffer
  named BUFFER-NAME if it exists, and switches to the previous
  buffer if we are currently visiting buffer BUFFER-NAME.

  The SWITCH-CONT argument is a function which, if given, is called
  after the buffer has been created or switched to.  This allows
  running further actions that setup the state of the buffer or
  modify it."
  (interactive)
  (let ((target-buf (get-buffer buffer-name)))
    (prin1 target-buf)
    (cond
     ((equal (current-buffer) target-buf)
      (switch-to-buffer nil))
     (target-buf
      (switch-to-buffer target-buf)
      (if switch-cont (funcall switch-cont)))
     (t
      (funcall buffer-create-fn)
      (if switch-cont (funcall switch-cont))))))

;; ============================================================================
;;  Prevent the cursor from going into the minibuffer prompt.
;;  Also custom functions set to have Emacs handle windows better.
;; ============================================================================

(customize-set-variable 'minibuffer-prompt-properties
			(quote (read-only t cursor-intangible t face minibuffer-prompt)))

;; https://protesilaos.com/emacs/dotemacs#h:50f8b1e4-b14e-453f-a37e-1c0e495ab80f
(use-package window ;; window.el
  :ensure nil
  :config
  (defvar base-number 1
    "Number for basic window resizing operations.")
  (defvar precision-number 1
    "A more precise number for fine-grained window-resizing.")

  (defun set-base-number (arg)
    "Set base-multiplier to ARG."
    (interactive "nEnter base multiplier: ")
    (setq base-number (max 1 arg)))

  (defun set-precision-number (arg)
    "Set precision-multiplier to ARG."
    (interactive "nEnter precision multiplier: ")
    (setq precision-number (max 1 arg)))

  (defun Angelique!--enlarge-window ()
    "Resize window height using multipliers."
    (interactive)
    (enlarge-window (* base-number
		       precision-number)))

  (defun Angelique!--enlarge-window-horizontally ()
    "Resize window height using multipliers."
    (interactive)
    (enlarge-window-horizontally (* base-number
				    precision-number)))

  (defun Angelique!--shrink-window ()
    "Resize window height using multipliers."
    (interactive)
    (shrink-window (* base-number
		      precision-number)))

  (defun Angelique!--shrink-window-horizontally ()
    "Resize window height using multipliers."
    (interactive)
    (shrink-window-horizontally (* base-number
				   precision-number)))

  (transient-define-prefix Angelique!--window-resize-transient ()
    "Transient layout for minute resizing of windows.
This is preferably activated through Angelique!--window-control!"
    ;; this is to prevent transient from exiting after command input.
    :transient-suffix 'transient--do-stay
    ["Resize windows..."
     ["Parameters"
      ("u" "set-base-number" set-base-number)
      ("y" "set-precision-number" set-precision-number)]
     ["Resize"
      ("n" "Widen" Angelique!--enlarge-window-horizontally)
      ("e" "Heighten" Angelique!--enlarge-window)
      ("i" "Shrink" Angelique!--shrink-window)
      ("o" "Narrow" Angelique!--shrink-window-horizontally)]])
  :custom
  (kill-buffer-quit-windows t)
  (even-window-sizes nil)
  (display-buffer-alist
   '(("\\`\\*\\(Help\\|helpful .*\\|Apropos\\|Occur\\|Dogears.*\\|Org.*\\)\\*\\'"
      (display-buffer-at-bottom)
      (window-height . 0.35))
     ("\\*Embark Collect: .*\\*"
      (display-buffer-at-bottom)
      (window-height . 0.3))
     ("\\*Embark Actions\\*"
      (display-buffer-at-bottom)
      (window-height . fit-window-to-buffer)
      (window-parameters. ((no-other-window . t)
			   (mode-line-format . none)))))))

(use-package window-x
  :ensure nil
  :config
  (transient-define-prefix Angelique!--window-control! ()
    "A better interface for window resizing and layout control."
    ;; this is to prevent transient from exiting after command input.
    :transient-suffix 'transient--do-stay
    ["Angelique! window control..."
     ["Rotate"
      ("n" "rotate-windows" rotate-windows)
      ("e" "rotate-windows-back" rotate-windows-back)
      ("i" "window-layout-rotate-clockwise" window-layout-rotate-clockwise)
      ("o" "window-layout-rotate-anticlockwise" window-layout-rotate-anticlockwise)]
     ["Flip & Transpose"
      ("l" "window-layout-flip-leftright" window-layout-flip-leftright)
      ("u" "window-layout-flip-topdown" window-layout-flip-topdown)
      ("y" "window-layout-transpose" window-layout-transpose)]
     ["Splitting & Closing"
      ("s" "split-window-right" split-window-right :transient nil)
      ("S" "split-window-below" split-window-below :transient nil)
      ("d" "delete-window" delete-window :transient nil)
      ("D" "kill-buffer-and-window" kill-buffer-and-window :transient nil)]
     ["Movement & buffer navigation."
      ("b" "switch-to-buffer" switch-to-buffer :transient nil)
      ("," "previous-buffer" previous-buffer)
      ("." "next-buffer" next-buffer)]
     ["Tabs"
      ("t" "tab-new" tab-new :transient nil)
      ("T" "tab-close" tab-close :transient nil)
      ("k" "tab-bar-switch-to-tab" tab-bar-switch-to-tab :transient nil)
      ("r" "tab-bar-switch-to-recent-tab" tab-bar-switch-to-recent-tab :transient nil)
      ("O" "tab-bar-move-window-to-tab" tab-bar-move-window-to-tab :transient nil)]
     ["Resize & Balance"
      (";" "resize-windows" Angelique!--window-resize-transient)
      (":" "balance-windows" balance-windows)]
     ["Misc"
      ("C-u" "universal-argument" universal-argument)]])
  :bind ("M-;" . Angelique!--window-control!))

(use-package ace-window
  :ensure t
  :commands ace-window
  :custom
  (aw-background nil)
  (aw-keys '(?a ?r ?s ?t ?g ?m ?n ?e ?i))
  :bind ("M-o" . ace-window))

;; ============================================================================
;;  Better keyboard quit.
;; ============================================================================

(defun Angelique!--keyboard-quit-dwim ()

  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the
expected thing when the minibuffer is open.

Whereas we want it to close the minibuffer,
even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."

  (interactive)
  (cond ((region-active-p)
	 (keyboard-quit))
	((derived-mode-p 'completion-list-mode)
	 (delete-completion-window))
	((> (minibuffer-depth) 0)
	 (abort-recursive-edit))
	(t
	 (keyboard-quit))))

(define-key global-map (kbd "C-g") #'Angelique!--keyboard-quit-dwim)

;; ============================================================================
;;  Utilities.
;; ============================================================================

(use-package time
  :ensure nil
  :config
  (display-time-mode 1))

(use-package tab-bar
  :ensure nil
  :config
  (set-face-attribute 'tab-bar-tab-highlight nil
		      :background "#F4B6FF")

  (set-face-attribute 'tab-bar-tab nil
		      :family "VictorMono Nerd Font Mono"
		      :slant 'italic
		      :background "#2F0052"
		      :foreground "#F4B6FF"
		      :height 95)
  
  (set-face-attribute 'tab-bar-tab-inactive nil
		      :family "VictorMono Nerd Font Mono"
		      :slant 'italic
		      :background "#090819"
		      :foreground "#7984D1"
		      :height 95)

  (defun Angelique!--nerd-icons-tab-bar-tab-name-all ()
    "Generate tab name from buffers of all windows.
This function concatenates nerd-icons to the tab-names.
If there are more than two windows, separate them with a separator."
    ;;; bufs is stolen from the original tab-bar-tab-name-all.
    (let* ((bufs (delete-dups (mapcar #'window-buffer
                                      (window-list-1 (frame-first-window)
                                                     'nomini))))
	   (seperator (propertize "  ❤  " 'face
				  `(:height 1.1 :weight bold :foreground "#FF70F8")))
	   (names (mapcar (lambda (buf)
			    (let* ((name (buffer-name buf))
				   (icon (with-current-buffer buf
					   (nerd-icons-icon-for-buffer)))
				   (icon-str (if (stringp icon)
						 (concat icon " ") "")))
			      (concat icon-str name)))
			  bufs)))
      (if (< (length bufs) 4) (mapconcat #'identity names seperator)
	(format "%s%s(+%d)" (car names) seperator (1- (length bufs))))))

  (setq tab-bar-separator " ")
    
  (defun Angelique!--tab-bar-close-button ()
    "Propertize tab-bar-close-button to be more pretty <3"
    (interactive)
    (setq tab-bar-close-button
	  (propertize "  x "
		    'close-tab t
		    :help "Click to close tab"
		    'face '(:family "FiraCode Nerd Font Propo"
				    :height 90 :foreground "hotpink" :weight bold))))
  
  :bind ("C-M-<tab>" . tab-bar-switch-to-prev-tab)
  :custom
  (tab-bar-new-tab-choice "*dashboard*")
  (tab-bar-show 1)
  (tab-bar-auto-width nil)
  (tab-bar-tab-name-function #'Angelique!--nerd-icons-tab-bar-tab-name-all)
  :hook
  (elpaca-after-init . tab-bar-mode)
  (tab-bar-mode . Angelique!--tab-bar-close-button))

;; Occur
(use-package replace
  :ensure nil
  :config
  ;; https://www.masteringemacs.org/article/searching-buffers-occur-mode
  (defun Angelique!--get-buffers-matching-mode (mode)
    "Returns a list of buffers where their major-mode is equal to MODE."
    (let ((buffer-mode-matches '()))
      (dolist (buf (buffer-list))
	(with-current-buffer buf
	  (when (eq mode major-mode)
	    (push buf buffer-mode-matches))))
      (buffer-mode-matches)))

  (defun Angelique!--multi-occur-in-this-mode ()
    "Show all lines matching REGEXP in buffers with this major-mode."
    (interactive)
    (multi-occur
     (Angelique!--get-buffers-matching-mode major-mode)
     (car (occur-read-primary-args)))))

(use-package delsel
  :ensure nil
  :hook (elpaca-after-init . delete-selection-mode))

(use-package ultra-scroll
  :ensure (:host github :repo "jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 101 ; important!
	scroll-margin 0)
  (setq pixel-scroll-precision-interpolate-page t)
  :config (ultra-scroll-mode 1))

(use-package magit
  :ensure t
  :commands magit
  :after transient
  :config
  ;; https://mbork.pl/2025-05-12_Coloring_Git_output_in_Magit
  (setq magit-process-finish-apply-ansi-colors t))

(use-package forge
  :ensure t
  :after magit)

(use-package consult-gh
  :ensure t
  :defer 2)

(use-package symbol-overlay
  :ensure t
  :config
  (require 'symbol-overlay)
  (set-face-attribute 'symbol-overlay-default-face nil
		      :background "#89B4FA"
		      :foreground "#4B0082")
  (setq symbol-overlay-idle-time 0.25)
  :hook ((prog-mode
	  org-mode) . symbol-overlay-mode))

(use-package embark
  :ensure t
  :bind (("C-;" . embark-act)
	 ("C-:" . embark-dwim)
	 :map minibuffer-local-map
	 ("C-c C-c" . embark-collect)
	 ("C-c C-e" . embark-export))
  :config (set-face-attribute 'embark-target nil
			      :background "#4B0082"))

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package smartparens
  :ensure t
  :defer 1
  :config
  (smartparens-global-mode)
  (require 'smartparens-config)
  :hook
  (minibuffer-setup . smartparens-mode))

(use-package vundo
  :ensure t
  :commands (vundo vundo-popup-mode))

(use-package vterm
  :ensure t
  :commands (vterm vterm-other-window
		   vterm-module-compile
		   vterm-next-error-function)
  :config
  (setq vterm-timer-delay nil)
  (add-hook 'vterm-mode-hook
	    (lambda ()
	      (hl-line-mode nil)
	      (display-line-numbers-mode -1))))

(use-package eat
  :ensure t
  :commands eat
  :hook
  (eat-mode . (lambda ()
		   (hl-line-mode nil)
		   (display-line-numbers-mode -1))))

(use-package eshell
  :ensure nil
  :commands eshell
  :hook
  (eshell-load . eat-eshell-mode)
  (eshell-load . eat-eshell-visual-command-mode)
  (eshell-mode . (lambda ()
		   (hl-line-mode nil)
		   (display-line-numbers-mode -1))))

(use-package shell
  :ensure nil
  :commands shell
  :hook
  (shell-mode . (lambda ()
		   (hl-line-mode nil)
		   (display-line-numbers-mode -1))))

(use-package compile
  :ensure nil
  :custom
  (compilation-ask-about-save nil)
  :hook
  (compilation-filter . ansi-color-compilation-filter))

(use-package comint
  :ensure nil
  :custom
  (comint-terminfo-terminal "dumb")
  (comint-input-ring-size 2000))

(use-package comint-mime
  :ensure t
  :hook
  (shell-mode . comint-mime-setup)
  (inferior-python-mode . comint-mime-setup))

(use-package coterm
  :ensure t
  :hook
  (elpaca-after-init . coterm-mode)
  (elpaca-after-init . coterm-auto-char-mode))

(use-package which-key
  :ensure nil
  :commands which-key-mode
  :hook (elpaca-after-init . which-key-mode))

(use-package winner-mode
  :ensure nil
  :commands winner-mode
  :hook (elpaca-after-init . winner-mode))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package pdf-tools
  :ensure t)

(use-package webjump
  :ensure nil
  :bind ("M-s M-e" . webjump))

(use-package consult
  :ensure t
  :bind (;; A recursive grep
	 ("M-s M-g" . consult-ripgrep)
	 ("M-s M-G" . consult-grep)
	 ("M-s M-f" . consult-find)
	 ("M-s M-o" . consult-outline)
	 ("M-s M-l" . consult-line)
	 ("M-s M-m" . consult-man)
	 ("M-s M-b" . consult-buffer)))

(use-package consult-eglot
  :ensure (:host github
		 :repo "mohkale/consult-eglot"
		 :files (:defaults "extensions/consult-eglot-embark/*.el"))
  :after consult
  :config
  (with-eval-after-load 'embark
    (with-eval-after-load 'consult-eglot
      (require 'consult-eglot-embark)
      (consult-eglot-embark-mode))))

(use-package wgrep
  :ensure t
  :defer 1)

(use-package keycast
  :ensure t
  :config
  ;; "https://github.com/tarsius/keycast/issues/7"
  ;; Can't really be deactivated...will have to make do.
  (define-minor-mode keycast-mode
    "Show current command and keybinding in the modeline
(fix for use with doom-modeline)."
    :global t
    (if keycast-mode
	(add-hook 'pre-command-hook 'keycast--update t)
     (remove-hook 'pre-command-hook 'keycast--update)))
  (add-to-list 'global-mode-string '("" keycast-mode-line))
  :hook (elpaca-after-init . keycast-mode))

(use-package emms
  :ensure t
  :defer 2)

(use-package multiple-cursors
  :ensure t
  :custom
  (mc/always-run-for-all t)
  :config
  (defun Angelique--multiple-cursors-activation ()
    (interactive)
    (message (propertize
	      "🩷(🌸ᴗ͈ˬᴗ͈)🩷* Multiple-Cursors activated! 🩷(🌸ᴗ͈ˬᴗ͈)🩷*"
	      'face `(:foreground "#FF83FA"))))
    
  (defun Angelique--multiple-cursors-deactivation ()
    (interactive)
    (message (propertize
	      "🩵(💠ᴗ͈ˬᴗ͈)🩵* Multiple-Cursors deactivated! 🩵(💠ᴗ͈ˬᴗ͈)🩵*"
	      'face `(:foreground "cyan"))))
  
  (global-set-key
   (kbd "C-x C-n")
   (defhydra Angelique!--multiple-cursors-map
     (:pre Angelique--multiple-cursors-activation
      :post Angelique--multiple-cursors-deactivation
      :color red)
     ("n" mc/mark-next-like-this "next-like-this")
     ("e" mc/mark-next-like-this-word "next-like-this-word")
     ("i" mc/edit-lines "edit-lines")
     ("o" mc/mark-previous-like-this "previous-like-this"))))

(use-package dogears
  :ensure t
  :defer 1
  :custom
  (dogears-ignore-modes '(fundamental-mode
			  dogears-list-mode
			  helm-major-mode
			  messages-buffer-mode))
  (dogears-hooks '(imenu-after-jump-hook
		   before-save-hook))
  (dogears-idle 2)
  (dogears-line-width 30)
  :hook (elpaca-after-init . dogears-mode))

;; ============================================================================
;;  Better help functionality for emacs.
;; ============================================================================

(use-package helpful
  :ensure t
  :defer 2
  :custom
  (helpful-max-buffers 2)
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h x" . helpful-command)
  ("C-h o" . helpful-symbol)
  ("C-h ." . helpful-at-point))

;; ============================================================================
;;  Configuring the minibuffer...
;; ============================================================================

(use-package marginalia
  :ensure t
  :bind (("M-C" . marginalia-cycle)
	 :map minibuffer-local-map
	 ("M-C" . marginalia-cycle))
  :hook (elpaca-after-init . marginalia-mode))

(use-package vertico
  :ensure t
  :custom
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  :config
  (set-face-attribute 'vertico-current nil
		      :foreground "cyan")
  :hook (elpaca-after-init . vertico-mode))

(use-package orderless
  :ensure t
  :config
  (set-face-attribute 'orderless-match-face-0 nil
		    :background "unspecified"
		    :foreground "cyan")
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides nil))

(use-package recentf
  :ensure nil
  :bind ("C-x C-r" . recentf)
  :config
  (add-to-list 'recentf-exclude
	       (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
	       (recentf-expand-file-name no-littering-etc-directory))
  :init
  (setq recentf-max-menu-items 25)
  (setq recentf-max-saved-items 25)
  (recentf-mode)
  :hook
  (kill-emacs . recentf-save-list)
  (save-buffers-kill-terminal . recentf-save-list))

(use-package savehist
  :ensure nil
  :config
  (setq history-length 2000)
  :hook (elpaca-after-init . savehist-mode))

;; ============================================================================
;;  LSP completions go here... using eglot.  removed lsp-mode as i wanna
;;  go as built-in as possible for the time being. for debugging, i feel like
;;  dap-mode relies on dependencies i dont really need? (treemacs)
;;  dape seems to be nice.
;; ============================================================================

(use-package cape
  :ensure t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; Add to the global default value of `completion-at-point-functions'
  ;; which is used by `completion-at-point'
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-history)
  (add-hook 'completion-at-point-functions #'cape-keyword))

(use-package corfu
  :ensure t
  :hook (elpaca-after-init . global-corfu-mode)
  :bind (:map corfu-map
	      ("<tab>" . corfu-complete)
	      ("TAB" . corfu-complete))
  :config
  (set-face-attribute 'corfu-current nil
		      :background "#120333"
		      :foreground "#B2FFFF")
  (set-face-attribute 'corfu-border nil
		      :background "#89B4FA")
  (set-face-attribute 'corfu-bar nil
		      :background "#B2FFFF")
  (setq tab-always-indent 'complete)
  (setq corfu-preview-current nil)
  (setq corfu-min-width 10)
  (setq corfu-preselect 'prompt)
  (setq corfu-quit-no-match t)
  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1) ; shows documentation after 'corfu-popupinfo-delay'
  ;; Sort by input history (no need to modify 'corfu-sort-function')
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package tempel
  :ensure t
  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
		(cons #'tempel-complete
		      completion-at-point-functions)))
  :hook
  ((prog-mode
    conf-mode
    text-mode) . tempel-setup-capf))

(use-package tempel-collection
  :ensure t
  :after tempel)

(use-package eglot
  :ensure nil
  :config
  (setq completion-category-overrides '((eglot (styles orderless))
					(eglot-capf (styles orderless))))
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (add-to-list 'eglot-server-programs
	       '((angelique-c-mode
		  angelique-c++-mode) . ("clangd")))
  :init
  (defun eglot-setup-completion ()
    (setq-local completion-at-point-functions
		(list (cape-capf-super
		       #'eglot-completion-at-point
		       #'tempel-complete
		       #'cape-file
		       #'cape-dabbrev))))
  :hook
  (eglot-managed-mode . eglot-setup-completion)
  ((ess-mode
    python-ts-mode
    c-ts-mode
    c++-ts-mode
    angelique-c-mode
    angelique-c++-mode
    rust-ts-mode) . eglot-ensure))

(use-package eldoc-box
  :ensure t
  :config
  (set-face-attribute 'eldoc-box-border nil
		      :background "#89B4FA")
  :hook
  (emacs-lisp-mode . eldoc-box-hover-mode)
  (eglot-managed-mode . eldoc-box-hover-mode))

(use-package flymake
  :ensure nil
  :commands flymake-mode
  :custom
  (flymake-indicator-type 'fringes)
  ;; (flymake-show-diagnostics-at-end-of-line 'fancy)
  :hook
  (prog-mode . flymake-mode)
  ;; getting annoyed by all the warnings flymake is generating
  ;; because of the byte-compiler. so turning it off for peace of mind...
  (emacs-lisp-mode . (lambda ()
			 (remove-hook 'flymake-diagnostic-functions #'elisp-flymake-byte-compile t))))

(use-package flymake-ruff
  :ensure t
  :hook (eglot-managed-mode . flymake-ruff-load))

(use-package dape
  :ensure t
  :commands dape
  :config
  (dape-breakpoint-global-mode))

(use-package sideline-flymake
  :ensure t)

(use-package sideline-eglot
  :ensure t)

(use-package sideline
  :ensure t
  :hook
  (flymake-mode . sideline-mode)
  (eglot-managed-mode . sideline-mode)
  :init
  (setq sideline-backends-right '(sideline-flymake
				  sideline-eglot))
  (setq sideline-order-right 'up
	sideline-display-backend-name t))

(use-package breadcrumb
  :ensure t
  :config
  (set-face-attribute 'breadcrumb-face nil
		      :height 95)
  :hook
  (elpaca-after-init . breadcrumb-mode))

(use-package blamer
  :ensure t
  :defer 2
  :bind
  ("C-c b" . blamer-show-posframe-commit-info)
  ("C-c B" . blamer-mode)
  :custom
  (blamer-view 'overlay)
  (blamer-idle-time 0.5)
  (blamer-min-offset 30))

(use-package completion-preview
  :ensure nil
  :bind
  (:map completion-preview-active-mode-map
	("TAB" . nil)
	("M-i" . nil)
	("C-<tab>" . completion-preview-insert))
  :hook
  (prog-mode . completion-preview-mode)
  (minibuffer-setup . completion-preview-mode))

(defun elisp-super-capf ()
  "Unifies `tempel-complete' with `elisp-completion-at-point' for elisp editing.
This is done using `cape-capf-super'."
  (setq-local completion-at-point-functions
	      (list (cape-capf-super
		     #'tempel-complete
		     #'elisp-completion-at-point
		     #'cape-dabbrev))))

(dolist (elisp-capf-hook '(emacs-lisp-mode-hook
			   lisp-interaction-mode-hook
			   ielm-mode-hook
			   org-mode-hook))
  (add-hook elisp-capf-hook #'elisp-super-capf))

(defun minibuffer-and-comint-super-capf ()
  "Enforce super CAPF's in the minibuffer and 'comint-mode'."
  (setq-local completion-at-point-functions
	      (list (cape-capf-super
		     #'cape-history
		     #'comint-completion-at-point))))

(defun Angelique!--comint-or-compilation-complete ()
  "Completion at point for comint or compilation buffers.
Temporarily disables read-only so Corfu/Cape can insert."
  (interactive)
  (let ((inhibit-read-only t))
    (completion-at-point)))

(define-key minibuffer-local-completion-map (kbd "TAB") #'corfu-complete)
(add-hook 'minibuffer-setup-hook #'minibuffer-and-comint-super-capf)
(define-key comint-mode-map (kbd "C-M-u") #'Angelique!--comint-or-compilation-complete)
(add-hook 'comint-mode-hook #'minibuffer-and-comint-super-capf)

;; ============================================================================
;;  Treesitter...
;;  Do take a look at treesit-auto for some ideas.
;; ============================================================================

(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4)
  (treesit-load-name-override-list '((c-sharp "libtree-sitter-csharp" "tree_sitter_c_sharp")))
  (treesit-extra-load-path '("~/.emacs.d/tree-sitter/"))
  :hook
  ((c-ts-mode
    c++-ts-mode
    angelique-c-mode
    angelique-c++-mode
    csharp-ts-mode
    python-ts-mode) . treesit-inspect-mode))

(use-package AngeliqueC
  :ensure nil
  :load-path "~/.emacs.d/Angelique!/AngeliqueC")

(defun Angelique!--treesit (language-specs)

  "Batch configure Tree-sitter for multiple `LANGUAGE-SPECS'.
This function will (hopefully) fallback to the original mode if there
is no ts-mode.  Each spec(s) is a list corresponding to the arguments
stated in the cons cells of `treesit-language-source-alist':

`(LANG (URL &OPTIONAL[REVISION SOURCE-DIR CC C++ COMMIT])
 EXT ORIG-MODE &OPTIONAL TS-MODE-NAME)'.

- `LANG': The specified programming language to install
	  the tree-sitter grammars.
	- `URL' : The source of the tree-sitter grammars.
  &OPTIONAL:
  -  `REVISION'  : Git tag or branch of the desired version.
		   Defaults to the latest default branch.
  -  `SOURCE-DIR': The tree-sitter parser (usually in `src').
		   Defaults to `src'.
  -  `CC'and`C++': Compilers for C and C++.
		   Defaults to \"cc\" and \"c++\" respectively.
  -  `COMMIT'    : If non-nil, checks out the commit hash
		   while cloning the repo.

For `auto-mode-alist' and `major-mode-remap-alist',
this function takes the following as arguments.

- `EXT'      : The file-extension of that particular programming
	       language that tree-sitter will be parsing.
- `ORIG-MODE': The original major-mode that tree-sitter will replace.
  &OPTIONAL:
  - `TS-MODE-NAME': This is for edge cases where the string does not
		    translate well to the particular tree-sitter `-ts-mode'
		    when passing through `lang', for example see `cpp' and `C++'."
  (interactive)
  (require 'treesit)
  (dolist (spec language-specs)
    (cl-destructuring-bind (lang (url &optional rev src cc c++ commit)
				 ext orig-mode &optional ts-mode-name) spec
      (add-to-list 'treesit-language-source-alist
		   `(,lang . (,url ,@(if rev (list rev) nil) ,@(if src (list src) nil)
				   ,@(if cc (list cc) nil) ,@(if c++ (list c++) nil)
				   ,@(if commit (list commit) nil))))
      (unless (treesit-ready-p lang) (treesit-install-language-grammar lang))
      (when (treesit-ready-p lang)
	(let ((ts-mode (or ts-mode-name (intern (format "%s-ts-mode" lang)))))
	  (add-to-list 'auto-mode-alist
		       `(,ext . (lambda () (if (fboundp ',ts-mode) (,ts-mode) (,orig-mode)))))
	  (when (fboundp ts-mode)
			  (add-to-list 'major-mode-remap-alist `(,orig-mode . ,ts-mode))))))))

(Angelique!--treesit
 '((python
    ("https://github.com/tree-sitter/tree-sitter-python")
    "\\.py\\'"
    python-mode)
   (c
    ("https://github.com/tree-sitter/tree-sitter-c")
    "\\.c\\'"
    c-mode)
   (cpp
    ("https://github.com/tree-sitter/tree-sitter-cpp")
    "\\.\\(cpp\\|hpp\\)\\'"
    c++-mode
    c++-ts-mode)
   ;; we can do a shim in c or we can do treesit-load-name-override-list
   (c-sharp
    ("https://github.com/tree-sitter/tree-sitter-c-sharp")
    "\\.cs\\'"
    csharp-mode
    csharp-ts-mode)
   (rust
    ("https://github.com/tree-sitter/tree-sitter-rust")
    "\\.rs\\'"
    rust-mode)
   (bash
    ("https://github.com/tree-sitter/tree-sitter-bash")
    "\\.sh\\'"
    sh-mode
    bash-ts-mode)
   (json
    ("https://github.com/tree-sitter/tree-sitter-json")
    "\\.json\\'"
    js-json-mode
    json-ts-mode)
   (javascript
    ("https://github.com/tree-sitter/tree-sitter-javascript")
    "\\.js\\'"
    js-mode
    js-ts-mode)
   (jsdoc
    ("https://github.com/tree-sitter/tree-sitter-jsdoc")
    "\\(\\.js[mx]\\|\\.har\\)\\'"
    js-mode
    js-ts-mode)))


;; ============================================================================
;;  Configure Dired / dirvish.
;; ============================================================================

(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode)
  :bind (("C-x j" . dired-jump)
	 ("C-x C-j" . dired-jump-other-window)
	 :map dirvish-mode-map
	 ("<tab>" . dirvish-subtree-toggle)
	 ("TAB" . dirvish-subtree-toggle))
  :config
  (set-face-attribute 'dirvish-hl-line nil
		      :inherit nil)
  (set-face-attribute 'dired-directory nil
		      :inherit font-lock-doc-markup-face
		      :foreground "#FBA0E3")
  (setq dirvish-mode-line-format
	'(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
	'(nerd-icons git-msg file-time file-size collapse subtree-state vc-state))
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
	"-l --almost-all --human-readable --group-directories-first --no-group")
  :hook
  (dired-mode . (lambda () (set-face-attribute 'dirvish-file-time nil
					       :inherit nil
					       :foreground "#A875FF"))))

(use-package trashed
  :ensure t
  :commands (trashed)
  :custom
  (trashed-use-header-line t)
  (trashed-sort-key '("Date deleted" . t))
  (trashed-date-format "%Y-%m-%d %H:%M:%S"))

;; ============================================================================
;;  Org-mode stuff goes here...need to ensure that it is the latest
;;  as built-in is older.
;; ============================================================================

(use-package org
  :ensure t
  :defer t
  :config
  (set-face-attribute 'org-document-title nil
		      :family "VictorMono Nerd Font Mono"
		      :weight 'bold
		      :slant 'italic
		      :foreground "#da70d6")
  
  (set-face-attribute 'org-level-1 nil
		      :family "VictorMono Nerd Font Mono"
		      :weight 'bold
		      :slant 'italic
		      :foreground "#FF70F8"
		      :height 133)

  (set-face-attribute 'org-level-2 nil
		      :foreground "#EAB3E4")

  (set-face-attribute 'org-document-info nil
		      :foreground "cyan")
  
  (defun Angelique!--org-last-modified ()
    "Insert or update the #+LAST_MODIFIED: line at point."
    (let ((insert-point (point))
	  (mod-str (format-time-string "#+LAST_MODIFIED: [%Y-%m-%d %A %H:%M]")))
      (if (re-search-forward "^#\\+LAST_MODIFIED:.*" nil t)
          (replace-match mod-str)
	(goto-char insert-point)
	(insert mod-str "\n"))))
  
  (defun Angelique!--org-update-last-modified ()
    "Ensure #+LAST_MODIFIED is updated just after #+DATE:.
Or, insert both after #+AUTHOR: if needed."
    (when (derived-mode-p 'org-mode)
      (let ((date-str (format-time-string "#+DATE: [%Y-%m-%d %A %H:%M]")))
	(save-excursion
	  (goto-char (point-min))
	  ;; Go to AUTHOR property...
	  (when (re-search-forward "^#\\+AUTHOR:.*" nil t)
	    ;; and move to line where DATE property is.
	    (forward-line 1)
	    (cond
	     ;; if DATE exists, move past it and update/insert LAST_MODIFIED
	     ((looking-at "^#\\+DATE:.*")
	      (forward-line 1)
	      (Angelique!--org-last-modified))
	     (t
	      (insert date-str "\n")
	      (Angelique!--org-last-modified))))))))

  (defun Angelique!--org-insert-youtube-transcript-async (url)
    "Asynchronously fetch YouTube auto-subs (en) via yt-dlp and insert Org-native links."
    (interactive "MYouTube URL: ")
    (let* ((id (if (string-match "v=\\([^&]+\\)" url)
                   (match-string 1 url)
		 url))
           (temp-prefix (make-temp-name "/tmp/org-ytdlp-"))
           (subtitle-file (concat temp-prefix ".en.srv1"))
           (cmd (list "yt-dlp" "--write-auto-sub" "--no-warnings"
                      "--sub-lang" "en" "--skip-download" "--sub-format" "srv1"
                      "-o" temp-prefix
                      (format "https://youtube.com/watch?v=%s" id))))
      (make-process
       :name "yt-dlp-transcript"
       :buffer "*yt-dlp-output*"
       :command cmd
       :noquery t
       :stderr "*yt-dlp-errors*"
       :sentinel
       (lambda (proc event)
	 (when (string= event "finished\n")
           (if (not (file-exists-p subtitle-file))
               (message "No transcript found for %s" id)
             (with-current-buffer (current-buffer)
               (save-excursion
		 (goto-char (point-max))
		 ;; 1) Insert a plain Org link to the video
		 (insert (format "[[https://youtu.be/%s][YouTube: %s]]\n\n" id id))
		 ;; 2) Insert table header
		 (insert "| Time | Text |\n|-\n")
		 ;; 3) Parse the .srv1 (XML) and emit each subtitle as a row
		 (let ((dom (xml-parse-file subtitle-file)))
                   (dolist (node (dom-by-tag dom 'text))
                     (let* ((start    (string-to-number (dom-attr node 'start)))
                            (ts       (format-time-string "%M:%S"
                                                          (seconds-to-time start)))
                            (link     (format "[[https://youtu.be/%s?t=%d][%s]]"
                                              id start ts))
                            (content  (replace-regexp-in-string
                                       "[ \n]+" " "
                                       (replace-regexp-in-string
					"&#39;" "'" (dom-text node)))))
                       (insert (format "| %s | %s |\n" link content))))))
               ;; cleanup
               (delete-file subtitle-file)
               (message "Transcript for %s inserted!" id))))))
      (message "Fetching transcript for %s…" id)))
  
  (transient-define-prefix Angelique!--org-navigation ()
    "Transient layout for easier Org navigation."
    ["Angelique! Org-navigation..."
     ["Org-roam"
      ("a" "org-roam-node-find" org-roam-node-find)
      ("r" "org-roam-node-insert" org-roam-node-insert)
      ("s" "org-roam-buffer-toggle" org-roam-buffer-toggle)
      ("t" "org-roam-ui-open" org-roam-ui-open)
      ("g" "org-roam-db-sync" org-roam-db-sync)]
     ["Babel & setting properties"
      ("e" "org-babel-tangle" org-babel-tangle)
      ("i" "org-insert-block-template" org-insert-block-template)
      ("d" "org-babel-demarcate-block" org-babel-demarcate-block)
      ("p" "org-set-property" org-set-property)]
     ["Search & Link"
      ("q" "org-ql-search" org-ql-search)
      ("f" "org-ql-find" org-ql-find)
      ("l" "org-store-link" org-store-link)
      ("L" "org-insert-last-stored-link" org-insert-last-stored-link)
      ("n" "org-insert-link" (lambda () (interactive) (call-interactively #'org-insert-link)))]
     ["Transclusion & Transcription"
      ("k" "org-transclusion-add" org-transclusion-add)
      ("h" "org-transclusion-remove" org-transclusion-remove)]
     ["Display images"
      ("o" "org-display-user-inline-images" org-display-user-inline-images)
      ("O" "org-link-preview" org-link-preview)]
     ["Misc"
      ("C-u" "universal-argument" universal-argument :transient t)
      ("z" "org-table-create-or-convert-from-region" org-table-create-or-convert-from-region :transient t)
      ("T" "org-timestamp" (lambda () (interactive) (call-interactively #'org-timestamp)))
      ("x" "org-mode-restart" org-mode-restart :transient t)]])

  ;;; Go to man pages in org mode.
  (with-eval-after-load 'org (require 'ol-man))
  
  :bind
  (("C-c o" . Angelique!--org-navigation)
   :map org-mode-map
   ;; remove org-deadline as binding...
   ("C-c C-d" . nil))
  :custom
  (org-startup-indented t)
  (org-pretty-entities t)
  (org-hide-emphasis-markers t)
  (org-hide-macro-markers t)
  (org-display-remote-inline-images 'cache)
  (org-image-max-width nil)
  (org-use-sub-superscripts '{})
  (org-directory "~/.emacs.d/Angelique!/庭園の王女/")
  (org-default-notes-file (concat org-directory "/notes.org"))
  
  (org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (eshell . t)
     (C . t)
     (R . t)
     (python . t)
     (org . t)))

  (org-structure-template-alist
   '(("a" . "export ascii")
     ("e" . "example")
     ("E" . "export")
     ("m" . "export md")
     ("h" . "export html")
     ("L" . "export latex")
     ("l" . "src emacs-lisp")
     ("p" . "src python")
     ("j" . "src json-ts")
     ("c" . "src C")
     ("s" . "src")
     ("S" . "src shell")
     ("n" . "notes")
     ("q" . "quote")
     ("u" . "update")
     ("v" . "verse")))

  (org-todo-keywords
   '((sequence
      "TODO"
      "DONE"
      "ONGOING"
      "SOMEDAY")))
  :hook
  (org-mode . org-link-preview-refresh)
  (before-save . Angelique!--org-update-last-modified))

(use-package org-remoteimg
  :ensure (:host github :repo "gaoDean/org-remoteimg")
  :after org
  :hook
  (org-mode . org-display-user-inline-images))

(use-package org-modern
  :ensure (:host github :repo "minad/org-modern")
  :custom
  ;; to enable org-modern-indent when org-indent is active.
  (org-modern-block-indent t)
  (org-modern-star 'replace)
  :hook
  (org-mode . org-modern-mode)
  (org-mode . org-indent-mode))

(use-package org-roam
  :ensure t
  :after org
  :custom
  (org-roam-directory (file-truename "~/.emacs.d/Angelique!/庭園の王女/"))
  (org-roam-extract-new-file-path "${slug}.org")
  (org-roam-capture-templates '(("d" "default" plain "%?"
				 :target
				 (file+head "${slug}.org"
					    "#+TITLE: ${title}\n#+AUTHOR:\n")
				 :unnarrowed t))))

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :commands org-roam-ui-mode)

(use-package org-ql
  :ensure t
  :after org)

(use-package org-appear
  :ensure t
  :after org
  :custom
  (org-appear-trigger 'on-change)
  :hook
  (org-mode . org-appear-mode))

;; https://github.com/nobiot/org-transclusion
(use-package org-transclusion
  :ensure t
  :after org
  :hook
  (org-mode . org-transclusion-mode))

(use-package org-transclusion-http
  :ensure (:repo "https://git.sr.ht/~ushin/org-transclusion-http")
  :after org-transclusion
  :config
  (with-eval-after-load 'org-transclusion
    (add-to-list 'org-transclusion-extensions 'org-transclusion-http)
    (require 'org-transclusion-http)))

;; (use-package org-scs)

;; ============================================================================
;;  Other programming modes here...
;; ============================================================================

(use-package ess
  :ensure t)

(use-package meson-mode
  :ensure t)

(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" "\\.clangd\\'"))

(use-package code-cells
  :ensure t
  :hook
  ((python-mode
    python-ts-mode) . code-cells-mode-maybe))

(use-package python
  :ensure nil
  :config
  (if (executable-find "~/.venv/bin/ipython3")
      (setq python-shell-interpreter "~/.venv/bin/ipython3")
    (setq python-shell-interpreter "~/.venv/bin/python3.13")))

;; ============================================================================
;;  gptel goes here...
;; ============================================================================

(use-package gptel
  :ensure t
  :defer 2)

;; ============================================================================
;;  nov.el...for epub support here...
;;  See: https://github.com/justinbarclay/.emacs.d?tab=readme-ov-file#novel
;; ============================================================================

(use-package nov
  :ensure t
  :defer t
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (with-eval-after-load 'nov
    (add-to-list 'shr-external-rendering-functions
		 '(i . (lambda (dom)
			 (shr-fontize-dom dom 'italic))))
    (add-to-list 'shr-external-rendering-functions
		 '(em . (lambda (dom)
			  (shr-fontize-dom dom 'italic))))
    (add-to-list 'shr-external-rendering-functions
		 '(strong . (lambda (dom)
			      (shr-fontize-dom dom 'bold))))
    (add-to-list 'shr-external-rendering-functions
		 '(b . (lambda (dom)
			      (shr-fontize-dom dom 'bold)))))
  :hook
  (nov-mode . (lambda ()
		(display-line-numbers-mode -1))))

;; ============================================================================
;;  Envrc which is evaluated last in this file.
;; ============================================================================

(use-package envrc
  :ensure t
  :commands envrc-global-mode
  :hook (elpaca-after-init . (lambda ()
			       (when (executable-find "direnv")
				 (envrc-global-mode)))))

(provide 'init)

;;; init.el ends here.
