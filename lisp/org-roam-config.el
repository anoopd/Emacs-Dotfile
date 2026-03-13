;;; org-roam-config.el --- Org-Roam configuration -*- lexical-binding: t -*-

;;; Commentary:
;; All org-roam related configuration including capture templates,
;; dailies, UI and consult integration

;;; Code:

;; ============================================================================
;; ORG-ROAM (Zettelkasten)
;; ============================================================================

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/org-roam/"))
  (org-roam-completion-everywhere t)
  (org-roam-dailies-directory "daily/")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n g" . org-roam-graph)
         ("C-c n d" . org-roam-dailies-goto-today)
         :map evil-normal-state-map
         ("SPC n l" . org-roam-buffer-toggle)
         ("SPC n f" . org-roam-node-find)
         ("SPC n i" . org-roam-node-insert)
         ("SPC n c" . org-roam-capture)
         ("SPC n g" . org-roam-graph)
         ("SPC n d" . org-roam-dailies-goto-today))
  :config
  (org-roam-db-autosync-mode)
  
;; Display tags prominently in node selection
  (setq org-roam-node-display-template
        (concat "${title:*} " 
                (propertize "${tags:30}" 'face 'org-tag)))
  
  

  ;; ── ML Engineer Daily Templates (Updated - March 2026) ─────────────────────
(setq org-roam-dailies-capture-templates
      '(("d" "Daily Log" entry
         "* %<%H:%M> Daily Log\n\n** 📚 Learning Today\n%?\n\n** ✅ Completed\n\n** ❓ Questions / Blockers\n\n** 📝 Tomorrow's Plan\n\n** ⏱ Time Spent"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d %A>\n#+filetags: :ml:daily:\n\n")
         :prepend t
         :empty-lines 1
         :clock-in t
         :clock-resume t)

        ("s" "Study Session" entry
         "* %<%H:%M> ≡ƒôû %^{Topic|Python|Math|SQL|PyTorch|Vertex AI|MLOps|GenAI}\n:PROPERTIES:\n:PHASE: %^{Phase|1-Foundation|2-ML-Core|3-VertexAI|4-MLE}\n:DURATION: %^{Duration|30min|1hr|1.5hr|2hr|3hr}\n:FOCUS: %^{Focus|Theory|Practice|Coding|Review}\n:END:\n\n** Notes\n%?\n\n** Key Takeaways\n\n** Questions / Blockers\n"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d %A>\n#+filetags: :ml:daily:\n\n"))

        ("p" "Practice Problem" entry
         "* %<%H:%M> ≡ƒÆ¬ %^{Problem Title}\n:PROPERTIES:\n:TYPE: %^{Type|LeetCode|Kaggle|SQL|Math|ML Concept}\n:DIFFICULTY: %^{Difficulty|Easy|Medium|Hard}\n:STATUS: %^{Status|Solved|Partial|Stuck|Review}\n:END:\n\n** Problem\n%?\n\n** My Approach\n\n** Solution\n#+begin_src python\n\n#+end_src\n\n** Learnings\n"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d %A>\n#+filetags: :ml:daily:\n\n"))

        ("x" "Exam Practice" entry
         "* %<%H:%M> ≡ƒÄ» %^{Exam|CDL|MLE} - %^{Topic}\n:PROPERTIES:\n:SCORE: %^{Practice Score}\n:END:\n\n** What I Studied\n%?\n\n** Key Takeaways\n\n** Weak Areas\n\n** Next Action Items\n"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d %A>\n#+filetags: :ml:daily:\n\n"))
        ))
  
 

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))

;; Consult integration for powerful searching
(use-package consult-org-roam
  :ensure t
  :after org-roam
  :config
  (consult-org-roam-mode 1)
  :bind
  ("C-c n s" . consult-org-roam-search)
  ("C-c n b" . consult-org-roam-backlinks)
  ("C-c n r" . consult-org-roam-forward-links))


(provide 'org-roam-config)
;;; org-roam-config.el ends here
