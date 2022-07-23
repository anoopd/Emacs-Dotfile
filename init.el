;;; init.el --- Basic Emac Config  -*- lexical-binding:t -*-
    ;; Straight.el package manager install
    (defvar bootstrap-version)
    (let ((bootstrap-file
	(expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	(bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
	(with-current-buffer
	    (url-retrieve-synchronously
	    "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	    'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)

    ;; Place customized settings in seperate file
    (setq custom-file (locate-user-emacs-file "custom.el"))
    (load custom-file 'noerror 'nomessage)


    ;; General Settings
(server-start)
(recentf-mode 1)
(save-place-mode 1)
(global-auto-revert-mode 1) ; Look for file changes outside emacs and update
(menu-bar-mode -1) ;Disable menu bar


(global-display-line-numbers-mode)
;; Explicitly define a width to reduce the cost of on-the-fly computation
(setq-default display-line-numbers-width 3)
(add-hook 'org-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook 'visual-line-mode)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq next-line-add-newlines t)
;; Thanks, but no thanks
(setq inhibit-startup-message t)
(xterm-mouse-mode 1)
(global-hl-line-mode 1)
(blink-cursor-mode 0)
;; When emacs asks for "yes" or "no", let "y" or "n" suffice
(setq use-short-answers t)
;; Blank scratch buffer
(setq initial-scratch-message "")
;;Keybinding ace-window
(global-set-key (kbd "M-o") 'ace-window)
(setq aw-dispatch-always t)


(eval-when-compile
  (defmacro my/embark-ace-action (fn)
    `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
       (interactive)
       (with-demoted-errors "%s"
         (require 'ace-window)
         (let ((aw-dispatch-always t))
           (aw-switch-to-window (aw-select nil))
           (call-interactively (symbol-function ',fn)))))))
(require 'embark)
(define-key embark-file-map (kbd "o") (my/embark-ace-action
find-file))
(define-key embark-buffer-map   (kbd "o") (my/embark-ace-action switch-to-buffer))
(define-key embark-bookmark-map (kbd "o") (my/embark-ace-action bookmark-jump))

;; BACKUPS/LOCKFILES --------
;; Don't generate backups or lockfiles.
(setq create-lockfiles nil
      make-backup-files nil
      ;; But in case the user does enable it, some sensible defaults:
      version-control t     ; number each backup file
      backup-by-copying t   ; instead of renaming current file (clobbers links)
      delete-old-versions t ; clean up after itself
      kept-old-versions 5
      kept-new-versions 5
      backup-directory-alist (list (cons "." (concat user-emacs-directory "backup/"))))



;; Org-mode specific key binding
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; Set time for 'DONE Tasks
(setq org-log-done 'time)

;; Set up the visible bell
(setq visible-bell t)


    ;; Packages installed using Straight.el
    (straight-use-package 'use-package) ;use straight.el for use-package expressions
    (straight-use-package 'magit)
    (straight-use-package 'magit-todos)
    (straight-use-package 'fzf)
    (straight-use-package 'elisp-def)
    (straight-use-package 'git-gutter)

(use-package evil
  :straight t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-want-Y-yank-to-eol t)
  :config
  (evil-mode 1))

(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode 1))

    (use-package which-key
    :straight t
    :defer 10
    :config
    (which-key-mode 1))

    (use-package avy
    :straight t
    :bind(("M-g" . avy-goto-word-0))
    )




    ;; Org Roam Config Start
    (use-package org-roam
    :straight t
    :custom
    (org-roam-directory "/Users/anoopd/org-roam")
    (org-roam-completion-everywhere t)
    :bind (("C-c n l" . org-roam-buffer-toggle)
	   ("C-c n f" . org-roam-node-find)
	   ("C-c n i" . org-roam-node-insert)
	   ("C-c n c" . org-roam-capture)
	   :map org-mode-map
	   ("C-M-i" . completion-at-point)
	   :map org-roam-dailies-map
	   ("Y" . org-roam-dailies-capture-yesterday)
	   ("T" . org-roam-dailies-capture-tomorrow)
	   ("v" . org-roam-dailies-capture-date)
	   
           ("d" . org-roam-dailies-find-date)
	   ("p" . org-roam-dailies-find-previous-note)
	   ("n" . org-roam-dailies-find-next-note)
	   ("t" . org-roam-dailies-find-today)
	   ("y" . org-roam-dailies-find-yesterday)
	   ("m" . org-roam-dailies-find-tomorrow))
	   
    :bind-keymap
    ("C-c n d" . org-roam-dailies-map)
    :config
    (setq org-roam-capture-templates
	'(("w" "wordpress" plain
	    "%?"
	    :if-new (file+head "php/wordpress/%<%Y%m%d%H%M%S>-${slug}.org"
				"#+title: ${title}\n#+filetags:WP")
	    :immediate-finish t
	    :unnarrowed t)

	  ("i" "ideas" plain "%?"
	   :if-new(file+head "ideas/%<%Y%m%d%H%M>-${slug}.org"
			      "#+title: ${title}\n#+filetags:ideas")
	   :immediate-finish t
	   :unnarrowed t) 


      
	  ("b" "bookmark" plain "%?"
	   :if-new(file+head "bookmarks/%<%Y%m%d%H%M>-${slug}.org"
			      "#+title: ${title}\n#+roam_key: ${ref}\n#+roam_tags: bookmark")
	   :immediate-finish t
	   :unnarrowed t) 

	    ("b" "blog" plain "%?"
	    :if-new
	    (file+head "blog/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags:blog")
	    :immediate-finish t
	    :unnarrowed t)
	    
	    ("s" "symfony" plain "%?"
	    :if-new
	    (file+head "php/symfony/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags:Symfony")
	    :immediate-finish t
	    :unnarrowed t)
	    
	    ("j" "javascript" plain "%?"
	    :if-new
	    (file+head "javascript/javascript/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
	    :immediate-finish t
	    :unnarrowed t)

	    ("r" "react" plain "%?"
	    :if-new
	    (file+head "javascript/react/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags:React")
	    :immediate-finish t
	    :unnarrowed t)

	    ("d" "deno" plain "%?"
	    :if-new
	    (file+head "javascript/deno/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags:React")
	    :immediate-finish t
	    :unnarrowed t)
	    
	    ("s" "swelte" plain "%?"
	    :if-new
	    (file+head "javascript/swelte/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags:React")
	    :immediate-finish t
	    :unnarrowed t)

	    ("p" "python" plain "%?"
	    :if-new
	    (file+head "python/python/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags:Python")
	    :immediate-finish t
	    :unnarrowed t)

	    ("n" "numpy" plain "%?"
	    :if-new
	    (file+head "python/numpy/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags:Numpy")
	    :immediate-finish t
	    :unnarrowed t)

	    ("a" "pandas" plain "%?"
	    :if-new
	    (file+head "python/pandas/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags:Pandas")
	    :immediate-finish t
	    :unnarrowed t)

	    ("c" "woocommerce" plain "%?"
	    :if-new
	    (file+head "php/woocommerce/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags:WooCommerce")
	    :immediate-finish t
	    :unnarrowed t)
	    
    ("e" "emacs" plain "%?"
	    :if-new
	    (file+head "lisp/emacs/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags:Emacs")
	    :immediate-finish t
	    :unnarrowed t)

	    ("g" "Gutenberg" plain "%?"
	    :if-new
	    (file+head "php/gutenberg/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags:Gutenberg")
	    :immediate-finish t
	    :unnarrowed t)))

    (cl-defmethod org-roam-node-type ((node org-roam-node))
  "Return the TYPE of NODE."
  (condition-case nil
      (file-name-nondirectory
       (directory-file-name
        (file-name-directory
         (file-relative-name (org-roam-node-file node) org-roam-directory))))
    (error "")))

    (setq org-roam-node-display-template
      (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

    (require 'org-roam-dailies)
    (org-roam-setup)
    )

    ;; Experimental org-roam End


    (use-package org-roam-ui
    :straight
	(:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
	:after org-roam
    ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
    ;;         a hookable mode anymore, you're advised to pick something yourself
    ;;         if you don't care about startup time, use
    ;;  :hook (after-init . org-roam-ui-mode)
	:config
	(setq org-roam-ui-sync-theme t
	    org-roam-ui-follow t
	    org-roam-ui-update-on-save t
	    org-roam-ui-open-on-start t))


    ;; Completion using Vertico
    (defun dw/minibuffer-backward-kill (arg)
    "When minibuffer is completing a file name delete up to parent
    folder, otherwise delete a word"
    (interactive "p")
    (if minibuffer-completing-file-name
	;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
	(if (string-match-p "/." (minibuffer-contents))
	    (zap-up-to-char (- arg) ?/)
	    (delete-minibuffer-contents))
	(backward-kill-word arg)))

    (use-package vertico
    :straight '(vertico :host github
			:repo "minad/vertico"
			:branch "main")
    :bind (:map vertico-map
	    ("C-j" . vertico-next)
	    ("C-k" . vertico-previous)
	    ("C-f" . vertico-exit)
	    :map minibuffer-local-map
	    ("M-h" . dw/minibuffer-backward-kill))
    :custom
    (vertico-cycle t)
    :custom-face
    (vertico-current ((t (:background "#3a3f5a"))))
    :init
    (vertico-mode))

    (use-package savehist
    :straight t
    :init
    (savehist-mode))


    (use-package orderless
	:straight t
	:init
    (setq completion-styles '(orderless)
	    completion-category-defaults nil
	    completion-category-overrides '((file (styles . (partial-completion))))))

    (defun dw/get-project-root ()
    (when (fboundp 'projectile-project-root)
	(projectile-project-root)))

    ;; Example configuration for Consult
    (use-package consult
    :straight t

    :init
    :config
    )

(use-package consult-dir
  :straight t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map 
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package ace-window
  :straight t
  )

(use-package consult-org-roam
   :straight t
   :init
   (require 'consult-org-roam)
   ;; Activate the minor-mode
   (consult-org-roam-mode 1)
   :custom
   (consult-org-roam-grep-func #'consult-ripgrep)
   :config
   ;; Eventually suppress previewing for certain functions
   (consult-customize
    consult-org-roam-forward-links
    :preview-key (kbd "M-."))
   :bind
   ("C-c n e" . consult-org-roam-file-find)
   ("C-c n b" . consult-org-roam-backlinks)
   ("C-c n r" . consult-org-roam-search))


(use-package marginalia
    :after vertico
    :straight t 
    :custom
    (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
    :init
    (marginalia-mode))

(use-package embark
  :straight t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)
   ("C->" . embark-become)
    :map minibuffer-local-map 
   ("C-d" . embark-act)) ;; alternative for `describe-bindings'
      
  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  
  :config
;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                nil
                 (window-parameters (mode-line-format . none))))
  ;; Show Embark actions via which-key
 (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

    (use-package solarized-theme
	:straight t
;;	:config
;;	(load-theme 'solarized-dark t))
)


;; Matching Parenthesis
(use-package paren
  :straight t
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

;; Org Superstar
(use-package org-superstar
    :straight t
    :after org
    :hook (org-mode . org-superstar-mode)
    :config
      (set-face-attribute 'org-superstar-header-bullet nil :inherit 'fixed-pitched :height 180)
    :custom
    ;; set the leading bullet to be a space. For alignment purposes I use an em-quad space (U+2001)
    (org-superstar-headline-bullets-list '(;; Original ones nicked from org-bullets
    "◉"
    "○"
    "✸"
    "✿"))
    (org-superstar-todo-bullet-alist '(("DONE" . ?✔)
                                       ("TODO" . ?⌖)
                                       ("ISSUE" . ?)
                                       ("BRANCH" . ?)
                                       ("FORK" . ?)
                                       ("MR" . ?)
                                       ("MERGED" . ?)
                                       ("GITHUB" . ?A)
                                       ("WRITING" . ?✍)
                                       ("WRITE" . ?✍)
                                       ))
    (org-superstar-special-todo-items t)
    (org-superstar-leading-bullet "")
    )

;; Remove Gap on new headings
(setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

(use-package org-appear
  :straight t
  :commands (org-appear-mode)
  :hook (org-mode . org-appear-mode)
  :init
  (setq org-hide-emphasis-markers t)
  (setq org-appear-autoemphasis t)
 ; (setq org-appear-autolinks t) 
  (setq org-appear-autosubmarkers t)) ; enable on sub and superscript

;;; Org-ql
(use-package org-ql
  :straight t
  )



(use-package org-fancy-priorities
  :straight t
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))
    

;; For Emacs-Lisp Coding
(use-package flycheck
  :straight t
  :init (global-flycheck-mode))

(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook #'elisp-def-mode))

;(add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode)


(use-package vimish-fold
:straight t
:after evil)

(use-package evil-vimish-fold
:straight t
:after vimish-fold
:init
(setq evil-vimish-fold-mode-lighter " [..] ")
(setq evil-vimish-fold-target-modes '(prog-mode conf-mode text-mode))
:config
  (global-evil-vimish-fold-mode))


(use-package evil-collection
  :straight t
  :after evil
  :init
  (evil-collection-init))


(use-package forge
  :straight t
  :after magit)

(use-package doom-modeline
:straight t
:config
  (doom-modeline-mode)
  (setq doom-modeline-buffer-file-name-style 'auto ;; Just show file name (no path)
        doom-modeline-enable-word-count t
        doom-modeline-buffer-encoding nil
        doom-modeline-icon t ;; Enable/disable all icons
        doom-modeline-modal-icon nil ;; Icon for Evil mode
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-bar-width 3))

(use-package hide-mode-line
  :straight t
  :commands (hide-mode-line-mode))

(use-package shrink-path
  :straight t)

(use-package all-the-icons
:straight t)

;; Setup Org-roam-protocol
(require 'org-roam-protocol)

(setq org-roam-capture-ref-templates
    '(("r" "ref" plain "%a %i"
	:target (file+head "bookmarks/${slug}.org" "#+title: ${title}\n#+date: %t\n\n")
	:unnarrowed t)))


(setq org-todo-keywords
    '((sequence
	"TODO(t)"                    ;What needs to be done
	"NEXT(n)"                    ;A project without NEXTs is stuck
	"|"
	"DONE(d)")
	(sequence
	"REPEAT(e)"                    ;Repeating tasks
	"|"
	"DONE")
	(sequence
	"HOLD(h)"                    ;Task is on hold because of me
	"PROJ(p)"                    ;Contains sub-tasks
	"WAIT(w)"                    ;Tasks delegated to others
	"REVIEW(r)"                  ;Daily notes that need reviews
	"IDEA(i)"                    ;Daily notes that need reviews
	"|"
	"STOP(c)"                    ;Stopped/cancelled
	"EVENT(m)"                   ;Meetings
	)))

(use-package org-super-agenda
:straight t
:after org
)

;;; Prenstation using org mode
(use-package ox-reveal
:straight  t
)
(setq org-reveal-root "file:///Users/anoopd/Documents/Vid-Slides/reveal.js/dist/reveal.js")

;;; Install Nord theme
(use-package nord-theme
  :straight t
  :config
  (load-theme 'nord t))

;;; Keybinding for consult buffer
(global-set-key (kbd "C-x C-b") 'consult-buffer)

;;; org-package
(unless (package-installed-p 'org-present)
  (package-install 'org-present))


;;; visual-fill-column for centering org-mode files or presentations
(use-package visual-fill-column
  :straight t)
  
(setq visual-fill-column-width 90)
(setq-default  visual-fill-column-center-text t)


(defun my/org-present-start ()

  ;;Tweak font sizes
(setq-local face-remapping-alist '((default (:height 2.5) variable-pitch)
				   (header-line (:height 4.0) variable-pitch)
				   (org-document-title (:height 1.75) org-document-title)
				   (org-code (:height 1.55)  org-code)
				   (org-verbatim (:height 1.55) org-verbatim)
				   (org-block (:height 1.25) org-block)
				   (org-block-begin-line (:height 0.7) org-block)))

  
  ;; Start centering Content
  (visual-fill-column-mode 1)
  (visual-line-mode 1)
)
		



(defun my/org-present-stop ()
  ;; Reset font customizations
  (setq-local face-remapping-alist '((default variable-pitch default)))
  ;; Stop centering Content
  (visual-fill-column-mode 0)
  (visual-line-mode 0))

;;; Register hooks with org-present
(add-hook 'org-present-mode-hook 'my/org-present-start)
(add-hook 'org-present-mode-quit-hook 'my/org-present-stop)

;;; Configure web-mode
(use-package web-mode
  :straight t
  :config
  (require 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode)))

(setq web-mode-engines-alist
      '(("php"    . "\\.phtml\\'")
	("jsx"  . "/some/react/path/.*\\.js[x]?\\'")
        ("blade"  . "\\.blade\\."))
      )

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

(setq web-mode-style-padding 1)
(setq web-mode-script-padding 1)
(setq web-mode-block-padding 0)
(setq web-mode-comment-style 2)

;; Enable autopairing
(setq web-mode-enable-auto-pairing t)

(setq web-mode-enable-css-colorization t)
(setq web-mode-enable-block-face t)
(setq web-mode-enable-part-face t)
(setq web-mode-enable-comment-interpolation t)

;;; Lsp Mode
(use-package lsp-mode
  :straight t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (php-mode . lsp)
	 (web-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode)

(straight-use-package 'php-mode)
(straight-use-package 'rjsx-mode)
(straight-use-package 'typescript-mode)
(straight-use-package 'php-mode)

(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)
(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)
(straight-use-package 'prettier-js)
(straight-use-package 'tide)
(straight-use-package 'company)

(yas-global-mode 1)
(add-hook 'yas-minor-mode-hook (lambda ()
				 (yas-activate-extra-mode 'fundamental-mode)))
					 

(require 'tree-sitter)
(require 'tree-sitter-langs)
(require 'prettier-js)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;;; use company mode in all buffers
(add-hook 'after-init-hook 'global-company-mode)


;;; Add wordpress-mode
(add-to-list 'load-path "/Users/anoopd/.emacs.d/wordpress-mode/")
(require 'wordpress-mode)
(add-hook 'php-mode-hook (lambda()
			    (if (wp/exists) (wordpress-mode))))
