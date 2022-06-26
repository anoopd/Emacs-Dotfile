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

;; Place customized settings in seperate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


;; General Settings
(server-start)
(menu-bar-mode -1)
(add-hook 'prog-mode-hook 'global-display-line-numbers-mode)
(add-hook 'org-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook 'visual-line-mode)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq next-line-add-newlines t)
;; Thanks, but no thanks
(setq inhibit-startup-message t)
(xterm-mouse-mode 1)
(global-hl-line-mode 1)


;; Org-mode specific key binding
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)



;; Set up the visible bell
(setq visible-bell t)


;; Packages installed using Straight.el
(straight-use-package 'use-package) ;use straight.el for use-package expressions
(straight-use-package 'evil)
(straight-use-package 'magit)
(straight-use-package 'evil-nerd-commenter)
;;(straight-use-package 'zenburn-theme)
(straight-use-package 'fzf)


;; Setup Evil Mode
(require 'evil)
(evil-mode 1)

(use-package f
  :straight t
  )

(use-package which-key
  :straight t
  :defer 10
  :config
  (which-key-mode 1))

(use-package avy
:ensure t
:bind(("M-g" . avy-goto-word-0))
)

(use-package evil-nerd-commenter
  :straight t
  :init (evilnc-default-hotkeys))



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
         ("C-M-i" . completion-at-point))

   :config
  (org-roam-setup)
)


(setq org-roam-capture-templates
      '(("w" "wordpress" plain
         "%?"
         :if-new (file+head "php/%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
	
        ("j" "javascript" plain "%?"
         :if-new
         (file+head "javascript/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
	
        ("p" "python" plain "%?"
         :if-new
         (file+head "python/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :article:\n")
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

(use-package marginalia
  :after vertico
  :straight t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

    

(use-package solarized-theme
     :straight t
     :config
     (load-theme 'solarized-dark t))


    
;; Mode Line
(setq display-time-format "%l:%M %p %b %y"
      display-time-default-load-average nil)
;; Matching Parenthesis
(use-package paren
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
    (org-superstar-headline-bullets-list '(" "))
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
