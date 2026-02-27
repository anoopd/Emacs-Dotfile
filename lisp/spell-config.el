(elpaca jinx
  (setq jinx-languages "en_US")

  ;; Enable in text files
  (add-hook 'text-mode-hook #'jinx-mode)

  ;; Explicitly enable in org
  (add-hook 'org-mode-hook #'jinx-mode)

  ;; Enable in programming comments
  (add-hook 'prog-mode-hook #'jinx-mode)

;; Bind in normal state
  (with-eval-after-load 'evil
    (evil-define-key 'normal 'global (kbd "z=") #'jinx-correct)
    (evil-define-key 'normal 'global (kbd "gs") #'jinx-correct)

    ;; Optional: insert mode binding
    (evil-define-key 'insert 'global (kbd "C-;") #'jinx-correct)))

(provide 'spell-config)

