;;; package --- early-init.el  -*- lexical-binding: t -*-

;;; Commentary:
;;; Various optimizations and better defaults for Emacs...

;;; Code:

(use-package emacs
  :custom
  ;;  Elpaca needs both of the below variables set to nil to work properly.
  (package-enable-at-startup nil)
  (package-activate-all nil)

  ;;  Runtime optimizations as found in Doom Emacs's configuration.
  ;;  PERF: A second, case-insensitive pass over `auto-mode-alist' is time wasted.
  (auto-mode-case-fold nil)

  ;;  More performant rapid scrolling over unfontified regions. May cause brief
  ;;  spells of inaccurate syntax highlighting right after scrolling, which should
  ;;  quickly self-correct.
  (fast-but-imprecise-scrolling t)
  (redisplay-skip-fontification-on-input t)
  (highlight-nonselected-windows nil)
  (inhibit-compacting-font-caches t)

  ;;  Window and frame settings
  (frame-inhibit-implied-resize t)
  (frame-resize-pixelwise t)
  (window-resize-pixelwise t)
  
  ;;  Setup defaults for native-compilation
  (native-comp-speed 3)
  (native-comp-jit-compilation t)
  (package-native-compile t)
  
  ;;  Basic Augumentations
  (global-so-long-mode t)
  (load-prefer-newer t)

  ;;  Try to not let emacs keep old version of files if any.
  (delete-old-versions t)
  (kept-new-versions 5)
  (kept-old-versions 2)

  ;;  Language environment is as always unicode
  (set-language-environment "UTF-8")
  (default-input-method nil)

  ;;  Limits before garbage-collect
  (gc-cons-threshold 134217728) ;; 128MB
  (read-process-output-max (* 1024 1024))

  ;;  Disable warning levels
  (warning-minimum-level :emergency)
  (byte-compile-warnings '(not obsolete))
  (warning-suppress-log-types '((comp) (bytecomp))) ;; warnings are annoying!
  
  ;;  Text engine related settings
  (column-number-mode t)
  (size-indication-mode t)
  (display-line-numbers-type 'relative)
  (global-display-line-numbers-mode 1)    ;; line numbers
  (global-hl-line-mode +1)                ;; always highlight current line

  ;;  Minibuffer settings
  (enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode 1)
  
  ;;  Startup
  (inhibit-startup-screen t)
  (inhibit-startup-message t)
  (inhibit-startup-echo-area-message t)
  (initial-scratch-message nil)
  (confirm-kill-emacs #'y-or-n-p)

  :config
  ;;  Disable bidirectional text scanning.
  (setq bidi-inhibit-bpa t)
  (setq-default bidi-display-reordering 'left-to-right)
  (setq-default bidi-paragraph-direction 'left-to-right)
  (setq-default cursor-in-non-selected-windows nil)

  (dolist (mode '(menu-bar-mode scroll-bar-mode tool-bar-mode))
    (funcall mode -1))

  (fset 'yes-or-no-p 'y-or-n-p))

(provide 'early-init)

;;; early-init.el ends here.


