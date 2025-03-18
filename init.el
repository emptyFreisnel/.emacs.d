;;; package --- init.el -*- lexical-binding: t -*-

;;; Commentary:
;;  emptyFreisnel's [isabelrenata] personal Emacs config...

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

;; ============================================================================
;;  Auto garbage collection when emacs loses focus and using the minibuffer.
;;  See: https://github.com/MatthewZMD/.emacs.d/blob/master/init.el
;;  may or may not work with using daemon-mode...
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
;;  Compile-angel to make sure packages are natively compiled.
;; ============================================================================

(use-package compile-angel
  :ensure t
  :demand t
  :config
  (setq compile-angel-verbose t) ;; set to nil to silence compile-angel.
  (push "init.el" compile-angel-excluded-files)
  (push "early-init.el" compile-angel-excluded-files)
  (push "elisp-ts-mode.el" compile-angel-excluded-files)
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
  :hook
  (elpaca-after-init . doom-modeline-mode))

(use-package indent-bars
  :ensure t
  :hook ((prog-mode) . indent-bars-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package colorful-mode
  :ensure t
  :hook ((prog-mode) . colorful-mode))

;; ============================================================================
;;  Wish emacs can expose more properties to font attributes
;;  or let us set priorities for layering. As it stands the region is
;;  an overlay so it has higher priority over text properties. so setting
;;  something to foreground overrides font-lock syntax highlighting...
;; ============================================================================

;; using daemon-mode...use default-frame-alist instead of
;; set-frame-font.

(add-to-list 'default-frame-alist '(font . "Fira Code Nerd Font 11"))
(add-to-list 'default-frame-alist '(width . 120))
(add-to-list 'default-frame-alist '(height . 45))
(add-to-list 'default-frame-alist '(cursor-color ."cyan"))

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

;; ============================================================================
;;  Some personalisations that make me happy...
;;  Setting up Angelique! folder for my own custom
;;  functions and perhaps my own little packages.
;; ============================================================================

(setq custom-file null-device) ;; not using customize feature.

(require 'server)
(unless (server-running-p)
  (server-start))

(defvar Angelique! (expand-file-name "Angelique!/" user-emacs-directory))
(unless (file-exists-p Angelique!)
  (make-directory Angelique! t))

;; ============================================================================
;;  Prevent the cursor from going into the minibuffer prompt.
;; ============================================================================

(customize-set-variable 'minibuffer-prompt-properties
			(quote (read-only t cursor-intangible t face minibuffer-prompt)))

;; ============================================================================
;;  Home row keybindings...
;;  The Angelique! keybindings are transient...
;;  TODO: To switch to Canary keyboard layout.
;; ============================================================================

(defvar-keymap Angelique!--map
  :doc "♡(✿ᴗ͈ˬᴗ͈)♡*~May you have sweet dreams, princess!~*"
  "n" #'backward-char
  "C-n" #'backward-char ;; for shift-selection to work.
  "e" #'next-line
  "i" #'previous-line
  "o" #'forward-char
  "C-o" #'forward-char
  "C-l" #'recenter-top-bottom
  "C-u" #'undo
  "C-a" #'move-beginning-of-line
  "C-e" #'move-end-of-line
  "M-a" #'backward-sentence
  "M-e" #'forward-sentence
  "M-o" #'right-word
  "M-n" #'left-word
  "M-s" #'backward-sexp
  "M-t" #'forward-sexp
  "M-d" #'sp-forward-slurp-sexp
  "M-c" #'sp-backward-barf-sexp)

(defun Angelique!--normal-cursor ()
  "Cursor indicator for Angelique!"
  (interactive)
  (set-cursor-color "cyan")
  (setq-default cursor-type 'box)
  (set-face-attribute 'line-number-current-line nil
		      :foreground "cyan")
  (message "... Angelique! deactivated ... ♡(✿ᴗ͈ˬᴗ͈)♡* "))

(defun Angelique!--keybindings ()
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
(define-key global-map (kbd "C-c n") #'Angelique!--keybindings)

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

(use-package delsel
  :ensure nil
  :hook (elpaca-after-init . delete-selection-mode))
  
(use-package ultra-scroll
  :ensure (:host github :repo "jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 101 ; important!
	scroll-margin 0)
  (setq pixel-scroll-precision-interpolate-page t)
  :config
  (ultra-scroll-mode 1))

(use-package hydra
  :ensure t)

(use-package transient
  :ensure t)

(use-package magit
  :ensure t
  :commands magit
  :after transient)

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
  :commands (avy-goto-char-timer)
  :config
  (setq avy-timeout-seconds 0.4)
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
  :defer t)

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
  :bind
  ("M-s M-s" . consult-omni)
  ("M-s a" . consult-omni-apps)
  ("M-s e" . consult-omni-external-search)
  :config
  (require 'consult-omni-sources)
  (require 'consult-omni-embark)
  (consult-omni-sources-load-modules))

(use-package embark
  :ensure t
  :bind (("C-;" . embark-act)
	 :map minibuffer-local-map
	 ("C-c C-c" . embark-collect)
	 ("C-c C-e" . embark-export))
  :config
  (set-face-attribute 'embark-target nil
		      :background "#4B0082"))

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
  :commands yequake-toggle
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
  :hook
  (elpaca-after-init . which-key-mode))

(use-package winner-mode
  :ensure nil
  :hook
  (elpaca-after-init . winner-mode))

(use-package exec-path-from-shell
  :ensure t)

(use-package pdf-tools
  :ensure t)

(use-package minimap
  ;; its nice to have the minimap sometimes even tho i usually dont use it...
  :ensure t
  :defer t
  :commands minimap-mode
  :config
  (set-face-attribute 'minimap-current-line-face nil
		      :background "#EEAEEE")
  (set-face-attribute 'minimap-active-region-background nil
		      :background "#120333")
  :custom
  (minimap-update-delay 0)
  (minimap-window-location 'right))

;; ============================================================================
;;  Configuring the minibuffer...
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

;; ============================================================================
;;  LSP completions go here...
;;  TODO: supplement this using lsp-booster.
;; ============================================================================

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
  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1) ; shows documentation after 'corfu-popupinfo-delay'
  ;; Sort by input history (no need to modify 'corfu-sort-function')
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package flymake
  :ensure nil
  :custom
  (flymake-indicator-type 'fringes)
  :hook ((prog-mode) . flymake-mode))

(use-package yasnippet
  :ensure t
  :bind (:map yas-minor-mode-map
	      ("TAB" . nil))
  :config
  (setq yas-snippet-dirs
	'("~/ .emacs.d/snippets"
	  "~/.emacs.d/elpaca/builds/yasnippet-snippets/snippets"))
  :hook (elpaca-after-init . yas-global-mode))

(use-package yasnippet-snippets
  :ensure t)

(use-package yasnippet-capf
  :ensure t
  :demand t
  :bind ("M-n" . yasnippet-capf)
  :after cape
  :init
  (defun yasnippet-capf-dwim ()
    (add-to-list 'completion-at-point-functions #'yasnippet-capf))
  :hook
  ((emacs-lisp-mode
    python-ts-mode
    c-ts-mode
    c++-ts-mode) . yasnippet-capf-dwim))

(defun elisp-super-capf ()
  
  "Unifies `yasnippet-capf' with `elisp-completion-at-point' for elisp editing.
This is done using `cape-capf-super'."
  
  (setq-local completion-at-point-functions
	      (list (cape-capf-super
		     #'elisp-completion-at-point
		     #'yasnippet-capf
		     #'cape-dabbrev))))

(dolist (elisp-capf-hook '(emacs-lisp-mode-hook
			   lisp-interaction-mode-hook
			   ielm-mode-hook))
  (add-hook elisp-capf-hook #'elisp-super-capf))

(use-package cape
  :ensure t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; Add to the global default value of `completion-at-point-functions'
  ;; which is used by `completion-at-point'
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :bind-keymap
  ("C-c l" . lsp-command-map)
  :custom
  (lsp-completion-provider :none) ;; using corfu!
  (lsp-idle-delay 0.3)
  :init
  (defun orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))
  (defun lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
	  '(orderless))
    ;; Optionally configure the first word as flex filtered.
    (add-hook 'orderless-style-dispatchers #'orderless-dispatch-flex-first)
    ;; Optionally configure the cape-capf-buster.
    (setq-local completion-at-point-functions
		(list (cape-capf-buster #'lsp-completion-at-point)))
    (setq-local completion-at-point-functions
		(list (cape-capf-super
		       #'lsp-completion-at-point #'yasnippet-capf #'cape-dabbrev))))
  :hook
  (lsp-completion-mode . lsp-mode-setup-completion)
  (lsp-mode . lsp-enable-which-key-integration)
  ((python-ts-mode
    c-ts-mode
    c++-ts-mode
    rust-ts-mode) . lsp-deferred))

(use-package lsp-ui
  :ensure t
  :after lsp-mode)

(use-package lsp-pyright
  :ensure t
  :after lsp-mode
  :custom
  (lsp-pyright-multi-root nil)
  (lsp-pyright-langserver-command "basedpyright")
  :hook
  ((python-ts-mode). (lambda ()
		       (require 'lsp-pyright))))

(use-package treemacs
  :ensure t
  :commands treemacs)

(use-package lsp-treemacs
  :ensure t)

(use-package dap-mode
  :ensure t
  :config
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy))

(use-package completion-preview
  :ensure nil
  :bind
  (:map completion-preview-active-mode-map
	("TAB" . nil)
	("C-<tab>" . completion-preview-insert))
  :hook ((prog-mode) . completion-preview-mode))

;; ============================================================================
;;  Treesitter...
;;  Do take a look at treesit-auto for some ideas.
;; ============================================================================

;; This is my own self-defined mode for elisp with tree-sitter.
;; just for my own personal use...most certainly not production ready.

;; (use-package elisp-ts-mode
;;   :ensure ())

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
   (elisp
    ("https://github.com/Wilfred/tree-sitter-elisp")
    "\\.el\\'"
    emacs-lisp-mode)
   (c
    ("https://github.com/tree-sitter/tree-sitter-c")
    "\\.c\\'"
    c-mode)
   (cpp
    ("https://github.com/tree-sitter/tree-sitter-cpp")
    "\\.\\(cpp\\|hpp\\)\\'"
    c++-mode
    c++-ts-mode)
   (rust
    ("https://github.com/tree-sitter/tree-sitter-rust")
    "\\.rs\\'"
    rust-mode)))

;; ============================================================================
;;  Configure Dired / dirvish.
;; ============================================================================

(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode)
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

;; (use-package org-roam)

;; (use-package org-modern
;;  :ensure t)

;; ============================================================================
;;  Emacs application framework (eaf) for integrated browser
;;  and better image-viewer. Note that eaf needs sexpdata==1.0.0 and epc as
;;  dependencies. No choice but to use flag --break-system-packages i guess...
;;  Emacs needs to be compiled with gtk3 support too...without daemon-mode...
;; ============================================================================

(defvar eaf-build-dir (expand-file-name
		       "elpaca/builds/eaf/" user-emacs-directory))
(defvar eaf-repo-dir (expand-file-name
		      "elpaca/repos/emacs-application-framework/*" user-emacs-directory))
(defvar eaf-json-file (expand-file-name
		       "applications.json" eaf-build-dir))

(defun eaf-sync-build-dir-from-repo ()
  "Elpaca only builds .el files and link them to the Elpaca builds folder.
As EAF needs Python and its applications.json file to work....
This will sync the contents from the repo into the builds folder."
  (interactive)
  (unless (file-exists-p eaf-json-file)
    (start-process-shell-command "eaf-delete"
				 "*Messages*"
				 (format "rm -rf %s/eaf.el" eaf-build-dir))
    (start-process-shell-command "eaf-sync"
				 "*Messages*"
				 (format "cp -rf %s %s" eaf-repo-dir eaf-build-dir))
    (message "eaf-sync-build-dir-from-repo is successful!"))
  (when (file-exists-p eaf-json-file)
  (message "eaf-json-file found! Skipping sync...")))

(use-package eaf
  :ensure (:host github
		 :repo "emacs-eaf/emacs-application-framework")
  :demand (eaf-search-it)
  :bind
  ("M-s M-e" . eaf-search-it)
  :hook
  (elpaca-after-init . eaf-sync-build-dir-from-repo)
  :config
  (require 'eaf)
  (require 'eaf-browser)
  (require 'eaf-image-viewer)
  :custom
  (eaf-byte-compile-apps t)
  (eaf-browser-default-search-engine "duckduckgo")
  (eaf-browser-enable-adblocker t)
  (eaf-browser-remember-history nil)
  (eaf-browser-dark-mode nil))

;; ============================================================================
;;  Envrc which is evaluated last in this file.
;; ============================================================================
		   
(use-package envrc
  :ensure t
  :commands envrc-global-mode
  :hook
  (elpaca-after-init . (lambda ()
			 (when (executable-find "direnv")
			   (envrc-global-mode 1)))))

(provide 'init)

;;; init.el ends here.
