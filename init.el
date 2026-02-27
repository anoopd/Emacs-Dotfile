;;; init.el --- Minimal Org-Roam Configuration with Elpaca -*- lexical-binding: t -*-

;;; Commentary:
;; Clean, focused Emacs setup for org-roam note-taking
;; Package Manager: Elpaca
;; Features: Evil (Vim), Vertico (fuzzy finder), Org-roam, Olivetti (zen mode)

;;; Code:

;; ============================================================================
;; ELPACA BOOTSTRAP (Package Manager)
;; ============================================================================

;;; init.el --- Minimal Org-Roam Configuration with Elpaca -*- lexical-binding: t -*-

;; ============================================================================
;; ELPACA BOOTSTRAP (Latest Version)
;; ============================================================================

;;; init.el --- Minimal Org-Roam Configuration with Elpaca -*- lexical-binding: t -*-




(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; ============================================================================
;; ELPACA BOOTSTRAP (Latest Version)
;; ============================================================================

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
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

;; Install use-package support
(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;; Block until current queue processed
(elpaca-wait)

;; ============================================================================
;; REST OF YOUR CONFIG BELOW...
;; ============================================================================
;; ============================================================================
;; REST OF YOUR CONFIG BELOW...
;; ============================================================================

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'whisper)
(require 'keybindings)
(require 'org-agenda-config)
(require 'simple-clock-config)
(require 'spell-config)
(require 'org-roam-config)
;; ============================================================================
;; BASIC SETTINGS (Clean UI)
;; ============================================================================
;; Disable startup screen
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)

;; Clean UI
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq ring-bell-function 'ignore)

;; Better defaults
(setq-default
 cursor-type 'bar
 line-spacing 0.2
 truncate-lines nil
 word-wrap t
 fill-column 80)

;; Show column number
(column-number-mode 1)

;; Better scrolling
(setq scroll-margin 3
      scroll-conservatively 101
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

;; y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Keep folders clean
(setq backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save-list/" user-emacs-directory) t))
      create-lockfiles nil)

;; UTF-8 everywhere
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; Revert buffers when files change on disk
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)


(use-package zoxide
  :ensure t
  :bind (("C-c z" . zoxide-find-file)    ; Jump to a file in a frequent directory
         ("C-c d" . zoxide-cd))           ; Change the default-directory (cd)
  :config
  ;; This ensures zoxide.el uses your existing zoxide database
  (setq zoxide-db-file "~/.local/share/zoxide/db.json"))


(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 25
        recentf-max-saved-items 50)
  ;; Save the list every 5 minutes and on Emacs shutdown
  (run-at-time nil (* 5 60) 'recentf-save-list))

;; ============================================================================
;; THEME
;; ============================================================================

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-gruvbox t)
  (doom-themes-org-config))

;; Modeline
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 30)
  (doom-modeline-bar-width 4)
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-minor-modes nil)
  )
;; Icons (required for doom-modeline)
(use-package nerd-icons
  :ensure t)

;; ============================================================================
;; EVIL MODE (Vim Keybindings)
;; ============================================================================

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t
        evil-want-C-d-scroll t
        evil-want-integration t
        evil-want-keybinding nil
        evil-respect-visual-line-mode t
        evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  (evil-set-initial-state 'dashboard-mode 'emacs)
  (evil-set-initial-state 'elpaca-ui-mode 'emacs)
  
  ;; ESC quits everywhere
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-org
  :ensure t
  :after (evil org)
  :hook ((org-mode . evil-org-mode)
         (org-agenda-mode . evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; In your org-config.el
(with-eval-after-load 'org
  (setq org-agenda-files
        (list
         (expand-file-name "journey.org" "~/org-roam/")
         (expand-file-name "inbox.org" "~/org-roam/")
         (expand-file-name "roam-dailies/" "~/org-roam/"))))
;; ============================================================================
;; COMPLETION & NAVIGATION (Vertico + Consult + Orderless)
;; ============================================================================

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t
        vertico-resize nil
        vertico-count 15))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode)
  :config
  (setq marginalia-align 'right))

(use-package consult
  :ensure t
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("C-x C-r" . consult-recent-file)
         ("C-c g" . consult-ripgrep)
         :map evil-normal-state-map
         ("/" . consult-line)
         ("SPC b" . consult-buffer)
         ("SPC f" . find-file)
         ("SPC r" . consult-recent-file))
  :config
  (setq consult-narrow-key "<"
        consult-line-start-from-top t))

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings)))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))


;; ============================================================================
;; ORG MODE
;; ============================================================================

(use-package org
  :ensure nil ; Built-in
  :hook (org-mode . (lambda ()
                     (org-indent-mode)
                     (visual-line-mode)))
  :config
  (setq org-hide-emphasis-markers t
        org-pretty-entities t
        org-startup-indented t
        org-startup-with-inline-images t
        org-image-actual-width '(600)
        org-ellipsis " ▾"
        org-return-follows-link t
        org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0))

(use-package org-modern
  :ensure t
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("◉" "○" "●" "○" "●" "○" "●")
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.2
        org-modern-list '((43 . "➤")
                         (45 . "–")
                         (42 . "•"))
        org-modern-block-name
        '((t . t)
          ("src" "»" "«")
          ("example" "»–" "–«")
          ("quote" "❝" "❞"))))

(use-package visual-fill-column
  :ensure t
  :hook (org-mode . visual-fill-column-mode)
  :config
  (setq-default visual-fill-column-center-text t
                visual-fill-column-width 100))

;; ============================================================================
;; ZEN MODE
;; ============================================================================

(use-package olivetti
  :ensure t
  :config
  (setq olivetti-body-width 100
        olivetti-minimum-body-width 80)
  
  (defun my/toggle-zen-mode ()
    "Toggle distraction-free writing mode."
    (interactive)
    (if (bound-and-true-p olivetti-mode)
        (progn
          (olivetti-mode -1)
          (when (fboundp 'display-line-numbers-mode)
            (display-line-numbers-mode 1))
          (doom-modeline-mode 1))
      (progn
        (olivetti-mode 1)
        (when (fboundp 'display-line-numbers-mode)
          (display-line-numbers-mode -1))
        (doom-modeline-mode -1))))
  
  :bind (("C-c z" . my/toggle-zen-mode)
         :map evil-normal-state-map
         ("SPC z" . my/toggle-zen-mode)))

(use-package writeroom-mode
  :ensure t
  :config
  (setq writeroom-width 100
        writeroom-mode-line t
        writeroom-bottom-divider-width 0
        writeroom-maximize-window nil
        writeroom-fullscreen-effect 'maximized)
  :bind (("C-c w" . writeroom-mode)
         :map evil-normal-state-map
         ("SPC w" . writeroom-mode)))

;; ============================================================================
;; UTILITIES
;; ============================================================================

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Auto-save
(use-package super-save
  :ensure t
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t
        auto-save-default nil))

;; ============================================================================
;; CUSTOM KEYBINDINGS (Space Leader)
;; ============================================================================

(with-eval-after-load 'evil
  ;; Clear space in normal mode
  (evil-define-key 'normal 'global (kbd "SPC") nil)
  
  ;; File operations
  (evil-define-key 'normal 'global
    (kbd "SPC f f") 'find-file
    (kbd "SPC f s") 'save-buffer
    (kbd "SPC f r") 'consult-recent-file
    (kbd "SPC f d") 'dired)
  
  ;; Buffer operations
  (evil-define-key 'normal 'global
    (kbd "SPC b b") 'consult-buffer
    (kbd "SPC b k") 'kill-buffer
    (kbd "SPC b n") 'next-buffer
    (kbd "SPC b p") 'previous-buffer
    (kbd "SPC b s") 'save-buffer)
  
  ;; Window operations
  (evil-define-key 'normal 'global
    (kbd "SPC w v") 'split-window-right
    (kbd "SPC w s") 'split-window-below
    (kbd "SPC w d") 'delete-window
    (kbd "SPC w o") 'delete-other-windows
    (kbd "SPC w h") 'evil-window-left
    (kbd "SPC w j") 'evil-window-down
    (kbd "SPC w k") 'evil-window-up
    (kbd "SPC w l") 'evil-window-right)
  
  ;; Search
  (evil-define-key 'normal 'global
    (kbd "SPC s s") 'consult-line
    (kbd "SPC s g") 'consult-ripgrep
    (kbd "SPC s b") 'consult-buffer)
  
  ;; Quit
  (evil-define-key 'normal 'global
    (kbd "SPC q q") 'save-buffers-kill-terminal
    (kbd "SPC q Q") 'kill-emacs))

;; ============================================================================
;; PERFORMANCE
;; ============================================================================

(setq read-process-output-max (* 1024 1024)) ; 1MB
(setq inhibit-compacting-font-caches t)

;; ============================================================================
;; STARTUP
;; ============================================================================

(setq default-directory "~/org-roam/")

;; Elpaca finished message
(add-hook 'elpaca-after-init-hook
          (lambda ()
            (message "Elpaca finished! Ready to take notes 📝")))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(which-key)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Copy to windows clipboard using clip 
(setq select-enable-clipboard t)
(setq select-enable-primary t)

(defun wsl-copy-to-clipboard (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "clip.exe" nil "clip.exe")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function #'wsl-copy-to-clipboard)
