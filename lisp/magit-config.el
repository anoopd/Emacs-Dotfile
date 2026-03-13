;;; magit-config.el --- Magit configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Magit and Forge configuration for git workflow
;; Loaded from init.el via (require 'magit-config)

;;; Code:
;; Ensure transient is up to date before magit loads
(use-package transient
  :ensure t)

(elpaca-wait)

(use-package magit
  :ensure t
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c g"   . magit-file-dispatch))
  :custom
  (magit-auto-revert-mode t)
  (magit-save-repository-buffers 'dontask)
  (magit-diff-refine-hunk t)
  (magit-section-initial-visibility-alist '((untracked . show)
                                            (staged    . show)
                                            (unstaged  . show)))
  (magit-log-auto-more t)
  (magit-log-margin '(t age magit-log-margin-width t 18))
  :config
  ;; Evil keybindings for magit buffers
  (with-eval-after-load 'evil
    (evil-define-key 'normal magit-mode-map
      (kbd "SPC g g") 'magit-status
      (kbd "SPC g c") 'magit-commit
      (kbd "SPC g p") 'magit-push
      (kbd "SPC g f") 'magit-fetch
      (kbd "SPC g b") 'magit-branch
      (kbd "SPC g l") 'magit-log
      (kbd "SPC g d") 'magit-diff))

  ;; Forge for GitHub integration
  (use-package forge
    :ensure t
    :after magit
    :config
    (add-to-list 'forge-alist
                 '("github.com" "api.github.com"
                   "github.com" forge-github-repository))))

(provide 'magit-config)
;;; magit-config.el ends here
