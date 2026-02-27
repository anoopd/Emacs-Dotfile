;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;;; Commentary:
;; Optimizations loaded before init.el

;;; Code:

;; Increase garbage collection threshold during startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Restore after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024) ; 16MB
                  gc-cons-percentage 0.1)))

;; Disable package.el (using Elpaca instead)
(setq package-enable-at-startup nil)

;; Native compilation settings
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil
        native-comp-deferred-compilation t))

;; Disable UI elements early for faster startup
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(mode-line-format . 0) default-frame-alist)

;; Prevent unwanted runtime builds
(setq load-prefer-newer t)

;;; early-init.el ends here
