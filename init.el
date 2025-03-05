;;; package --- init.el -*- lexical-binding: t -*-

;;; Commentary:

;; ============================================================================

;;  See Protesilaos Stavrou excellent articles on Emacs...and minimal-emacs.

;;  Emacs is just too much sometimes...so uhm...will document as i go along...
;;  Not sure if i wanna convert this init.el file to org...org-babel-load-file
;;  but startup times tho? errrr let's see how the future holds....

;;  i am terrible at lisp... so these little comments are my lifesavers...
;;  dunno if anyone would actually read this but...isabel...you can do this...
;;  please be kind to yourself! whoever is reading this... you are loved <3

;; ============================================================================

;; Set up package manager...using Elpaca.
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

;; TODO: reminder to have ensure t be popped up upon completion of config.

;; ============================================================================
;;  Compile-angel to make sure packages are natively compiled.
;; ============================================================================

(use-package compile-angel
  :ensure t
  :demand t
  :config
  (setq compile-angel-verbose t) ;; set to nil to silence compile-angel.
  (push "init.el" compile-angel-excluded-files)
  (push "early-init.el" compile-angel-excluded-files)
  (compile-angel-on-load-mode)
  (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode))

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
  :hook ((prog-mode nix-mode) . indent-bars-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package colorful-mode
  :ensure t
  :hook ((prog-mode) . colorful-mode))

;; TODO: fix with better fonts for Org Mode and the like.
(set-frame-font "Fira Code Nerd Font 11" nil t)

(defun Angelique!--transparency ()
    "Toggle transparency."
    (interactive)
    (let ((alpha-transparency 75))
      (pcase (frame-parameter nil 'alpha-background)
	(alpha-transparency (set-frame-parameter nil 'alpha-background 100))
	(t (set-frame-parameter nil 'alpha-background alpha-transparency)))))

;; ============================================================================
;;  Wish emacs can expose more properties to font attributes
;;  or let us set priorities for layering. As it stands the region is
;;  an overlay so it has higher priority over text properties. so setting
;;  something to foreground overrides font-lock syntax highlighting...
;; ============================================================================

(add-hook 'elpaca-after-init-hook
	  (lambda ()
	    (set-face-attribute 'region nil
				:background "#4B0082") ;; indigo
	    
	    (set-face-attribute 'highlight nil
				:background "#EEAEEE")

	    (set-face-attribute 'lazy-highlight nil
				:background "#EEAEEE")

	    (set-face-attribute 'isearch nil
				:background "#EEAEEE")

	    (set-default 'cursor-type 'box)
	    (set-face-attribute 'cursor nil
				:background "cyan")

	    (set-face-attribute 'hl-line nil
				:background "#120333")

	    (set-face-attribute 'line-number-current-line nil
				:foreground "cyan")))

;; ============================================================================
;;  Preferred behaviour and custom functions.
;; ============================================================================

(setq custom-file null-device) ;; not using customize feature.

;; ============================================================================
;;  Prevent the cursor from going into the minibuffer prompt.
;; ============================================================================

(customize-set-variable
 'minibuffer-prompt-properties
 (quote (read-only t cursor-intangible t face minibuffer-prompt)))

;; ============================================================================
;;  Home row keybindings...
;;  The Angelique! keybindings are transient...
;;  TODO: To switch to Canary keyboard layout.
;; ============================================================================

;; This is left for discoverbility.
(defun repeatify (repeatify-map)
 "Set the `repeat-map' property on all commands bound in REPEATIFY-MAP."
 (named-let process ((keymap (symbol-value repeatify-map)))
   (map-keymap
    (lambda (_key cmd)
      (cond
       ((symbolp cmd) (put cmd 'repeat-map repeatify-map))
       ((keymapp cmd) (process cmd))))
    keymap)))

(defvar-keymap Angelique!--map
  :doc "♡(✿ᴗ͈ˬᴗ͈)♡*~May you have sweet dreams, princess!~*"
  "n" #'backward-char
  "C-n" #'backward-char ;; for shift-selection to work.
  "e" #'next-line
  "i" #'previous-line
  "o" #'forward-char
  "C-o" #'forward-char
  "l" #'recenter-top-bottom
  "u" #'undo
  "C-a" #'move-beginning-of-line
  "C-e" #'move-end-of-line
  "M-a" #'backward-sentence
  "M-e" #'forward-sentence
  "M-o" #'right-word
  "M-n" #'left-word
  "M-s" #'backward-sexp
  "M-t" #'forward-sexp
  "M-d" #'sp-slurp-hybrid-sexp
  "M-c" #'sp-backward-barf-sexp)

(defun Angelique!--normal-cursor ()
  "Cursor indicator for Angelique!"
  (interactive)
  (set-cursor-color "cyan")
  (setq-default cursor-type 'box)
  (set-face-attribute 'line-number-current-line nil
		      :foreground "cyan")
  (message "... Angelique! deactivated ... ♡(✿ᴗ͈ˬᴗ͈)♡* "))

(defun Angelique! ()
  "~Dream the good dream, like a good pretty-princess should!♡(✿ᴗ͈ˬᴗ͈)♡*~."
  (interactive)
  (set-cursor-color "#FF83FA")
  (setq-default cursor-type 'bar)
  (set-face-attribute 'line-number-current-line nil
		      :foreground "#FF83FA")
  (message "♡(✿ᴗ͈ˬᴗ͈)♡* Angelique! navigation activated!")
  (set-transient-map Angelique!--map
		     t ;; keep-pred = keymap will stay active until
		       ;; keys outside the keymap is pressed.
		     #'Angelique!--normal-cursor))

(define-key global-map (kbd "C-x m") nil)
(define-key global-map (kbd "M-n") #'Angelique!)
(define-key minibuffer-mode-map (kbd "M-n") nil)
(define-key minibuffer-mode-map (kbd "M-f") 'next-history-element)

(add-hook 'Info-mode-hook
	  (lambda ()
	    (define-key Info-mode-map (kbd "M-n") nil)))

(add-hook 'comint-mode-hook
	  (lambda ()
	    (define-key comint-mode-map (kbd "M-n") nil)))

(add-hook 'shell-command-mode-hook
	  (lambda ()
	    (define-key shell-command-mode-map (kbd "M-n") nil)))

;; ============================================================================
;;  Better keyboard quit.
;; ============================================================================

(defun fres/keyboard-quit-dwim ()
  
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
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'fres/keyboard-quit-dwim)

(use-package delsel
  :ensure nil
  :hook (elpaca-after-init . delete-selection-mode))
  
(use-package ultra-scroll
  :ensure (:host github :repo "jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 101 ; important!
	scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

;; ============================================================================
;;  Utilities.
;; ============================================================================

(use-package transient
  :ensure t)

(use-package magit
  :ensure t
  :commands magit
  :after transient
  :bind (:map magit-mode-map
	      ("M-n" . nil)
	      ("M-f" . magit-section-forward-sibling)
	      :map magit-log-mode-map
	      ("M-n" . nil)
	      ("M-f" . magit-section-forward-sibling)
	      :map git-commit-mode-map
	      ("M-n" . nil)
	      ("M-f" . git-commit-next-message)))

(use-package forge
  :ensure t
  :commands forge
  :after magit)

(use-package compat
  :ensure t)
    
(use-package ace-window
  :ensure t
  :commands ace-window
  :bind ("M-o" . ace-window))

;; See karthik's post on how to use avy.
(use-package avy
  :ensure t
  :config
  (setq avy-timeout-seconds 0.25)
  :bind ("M-r" . avy-goto-char-timer))

(use-package symbol-overlay
  :ensure t
  :bind ("M-;" . symbol-overlay-put)
  :config
  (require 'symbol-overlay)
  (set-face-attribute 'symbol-overlay-default-face nil
		      :background "unspecified"
		      :foreground "cyan")
  (setq symbol-overlay-idle-time 0.25)
  :hook
  (prog-mode . symbol-overlay-mode))

(use-package browser-hist
  :ensure t
  :commands browser-hist)

(use-package consult
  :ensure t
  :bind (;; A recursive grep
	 ("M-s M-g" . consult-ripgrep)
	 ("M-s M-f" . consult-find)
	 ("M-s M-o" . consult-outline)
	 ("M-s M-l" . consult-line)
	 ("M-s M-b" . consult-buffer)))

;;; See Sacha Chua's articles on consult-omni.
(use-package consult-omni
  :ensure  (:host github
		  :repo "armindarvish/consult-omni"
		  :branch "main"
		  :files (:defaults "sources/*.el"))
  :after (consult browser-hist)
  :bind ("M-s M-s" . consult-omni)
  :config
  (add-to-list 'load-path
	       "~/freisnel/.emacs.d/elpaca/builds/repo/consult-omni/sources")
  (require 'consult-omni-sources)
  (consult-omni-sources-load-modules))

(use-package embark
  :ensure t
  :bind (("C-;" . embark-act)
	 :map minibuffer-local-map
	 ("C-c C-c" . embark-collect)
	 ("C-c C-e" . embark-export)))

(use-package smartparens
  :ensure t
  :hook (prog-mode)
  :config
  (require 'smartparens-config))

(use-package embark-consult
  :ensure t)

(use-package sudo-edit
  :ensure t
  :commands sudo-edit)

(use-package vundo
  :ensure t
  :commands vundo)

(use-package vterm
  :ensure t
  :commands vterm
  :config
  (setq vterm-timer-delay nil)
  (add-hook 'vterm-mode-hook
	    (lambda ()
	      (hl-line-mode nil)
	      (display-line-numbers-mode -1)))'t)

(use-package yequake
  :ensure t
  :custom
  (yequake-frames
	'(("Yequake" .
	   ((width . 0.5)
	    (height . 0.5)
	    (alpha . 0.95)
	    (frame-parameters . ((undecorated . t)
				 (skip-taskbar . t))))))))
(use-package which-key
  :ensure nil
  :commands which-key-mode
  :diminish
  :hook
  (elpaca-after-init . which-key-mode))

;; ============================================================================
;;  Configure the minibuffer and LSP completions.
;; ============================================================================

(use-package marginalia
  :ensure t
  :hook (elpaca-after-init . marginalia-mode))

(use-package vertico
  :ensure t
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
  (recentf-mode 1))
 
(use-package savehist
  :ensure nil
  :hook (elpaca-after-init . savehist-mode))

(use-package corfu
  :ensure t
  :hook (elpaca-after-init . global-corfu-mode)
  :bind (:map corfu-map
	      ("<tab>" . corfu-complete)
	      ("M-n" . nil)   ;; using Angelique! as corfu-next.
	      ("M-p" . nil))  ;; using Angelique! as corfu-previous.
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
  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1) ; shows documentation after 'corfu-popupinfo-delay'
  ;; Sort by input history (no need to modify 'corfu-sort-function')
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package completion-preview
  :ensure nil
  :bind
  (:map completion-preview-active-mode-map
	("TAB" . nil)
	("C-<tab>" . completion-preview-insert))
  :hook ((prog-mode) . completion-preview-mode))

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
  (add-hook 'completion-at-point-functions #'cape-dict))

(use-package flymake
  :ensure nil
  :hook ((prog-mode emacs-lisp-mode) . flymake-mode))

(use-package yasnippet
  :ensure t)

(use-package lsp-mode
  :ensure t)

(use-package company
  :ensure t)

;; ============================================================================
;;  Treesitter...
;; ============================================================================

(require 'treesit)
(use-package treesit-auto
  :ensure t
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode 1))



;; ============================================================================
;;  Configure Dired / dirvish and turning on server mode.
;; ============================================================================

(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode)
  (server-start)
  :bind
  ( :map dirvish-mode-map
    ("<tab>" . dirvish-subtree-toggle)
    ("TAB" . dirvish-subtree-toggle))
  :config
  (set-face-attribute 'dirvish-hl-line nil
		      :inherit nil)
  (setq dirvish-mode-line-format
	'(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
	'(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
	"-l --almost-all --human-readable --group-directories-first --no-group"))

(use-package dired+
  :ensure (:host github :repo "emacsmirror/dired-plus"))

(use-package trashed
  :ensure t
  :commands (trashed)
  :custom
  (trashed-use-header-line t)
  (trashed-sort-key '("Date deleted" . t))
  (trashed-date-format "%Y-%m-%d %H:%M:%S"))

;; ============================================================================
;;  Org-mode stuff goes here...
;; ============================================================================

;; (use-package org
;;  :ensure nil)

;; ============================================================================
;;  Emacs application framework for integrated browser.
;; ============================================================================

;; (use-package eaf
;; :ensure (:host github
;;           :repo "emacs-eaf/emacs-application-framework"
;;           :files (:defaults ().)))

;; This should be in the last of the init.el file.

(use-package envrc
  :ensure t
  :hook
  (elpaca-after-init . envrc-global-mode))

(provide 'init)

;;; init.el ends here
