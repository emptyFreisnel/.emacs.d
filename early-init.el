;;; package --- early-init.el  -*- lexical-binding: t -*-

;;; Commentary:
;;; Various optimizations and better defaults for Emacs...

;;; Code:

;; ============================================================================
;;  for Elpaca
;; ============================================================================

(setq package-enable-at-startup nil)
(setq package-activate-all nil)

;; ============================================================================
;;  Optimizations
;;  Most of it is found in Doom Emacs's configuration.
;; ============================================================================

(global-so-long-mode 1)
(setopt native-comp-speed 3)

;; Disable bidirectional text scanning.
(setq-default bidi-display-reordering 'left-to-right
	      bidi-paragraph-direction 'left-to-right)

(setq auto-mode-case-fold nil)

(setq-default cursor-in-non-selected-windows nil)

(setq redisplay-skip-fontification-on-input t)

(setq highlight-nonselected-windows nil)

(setq bidi-inhibit-bpa t)

(setq inhibit-compacting-font-caches t)
(setq package-native-compile t)

(setq frame-inhibit-implied-resize t)
(setq frame-resize-pixelwise t)

(setq window-resize-pixelwise t)

(setq load-prefer-newer t)
(setq native-comp-jit-compilation t)

(setq gc-cons-threshold 134217728) ;; 128MB

(setq read-process-output-max (* 1024 1024))

;; ============================================================================
;;  Basic augumenations
;; ============================================================================

(dolist (mode '(menu-bar-mode scroll-bar-mode tool-bar-mode))
  (funcall mode -1)
  (when (daemonp)
    (add-hook 'after-make-frame-functions (lambda (frame)
					    (select-frame frame)
					    (funcall mode -1)))))

(column-number-mode t)

(size-indication-mode t)

(global-display-line-numbers-mode 1)    ;; line numbers
(global-hl-line-mode +1)                ;; always highlight current line

(fset 'yes-or-no-p 'y-or-n-p)

(setq warning-minimum-level :emergency)
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp))) ;; warnings are annoying!

(setq create-lockfiles nil)

(setq delete-old-versions t
      kept-new-versions 5
      kept-old-versions 2)

(setq set-language-environment "UTF-8")
(setq default-input-method nil)

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

(setq confirm-kill-emacs #'y-or-n-p)

;; ============================================================================
;;  Startup
;; ============================================================================

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)

;;; early-init.el ends here.


