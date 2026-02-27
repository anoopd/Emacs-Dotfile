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
  
(setq org-roam-capture-templates
      '(
        ;; ============ EXAM NOTES (with Title Heading) ============
        ("e" "Exam Preparation")
        
        ("ec" "CDE - Cloud Data Engineer" plain
         "* ${title}\n%?"
         :target (file+head "exams/cde/${slug}.org"
                            "#+title: ${title}\n#+filetags: :exam:cde:\n#+created: %U\n\n")
         :unnarrowed t)
        
        ("ed" "CDL - Cloud Digital Leader" plain
         "* ${title}\n%?"
         :target (file+head "exams/cdl/${slug}.org"
                            "#+title: ${title}\n#+filetags: :exam:cdl:\n#+created: %U\n\n")
         :unnarrowed t)
        
        ("em" "MLE - ML Engineer" plain
         "* ${title}\n%?"
         :target (file+head "exams/mle/${slug}.org"
                            "#+title: ${title}\n#+filetags: :exam:mle:\n#+created: %U\n\n")
         :unnarrowed t)
        
        ("ea" "ACE - Associate Cloud Engineer" plain
         "* ${title}\n%?"
         :target (file+head "exams/ace/${slug}.org"
                            "#+title: ${title}\n#+filetags: :exam:ace:\n#+created: %U\n\n")
         :unnarrowed t)
        
        ;; ============ PERMANENT NOTES (Keep existing) ============
        ("p" "Permanent Knowledge")
        
        ("pm" "ML Concept/Algorithm" plain
         "* Definition\n%?\n\n* Mathematical Foundation\n\\[\n\n\\]\n\n* Intuition\n\n* How It Works\n\n* Advantages\n- \n\n* Disadvantages\n- \n\n* Use Cases\n\n* Implementation\n#+begin_src python\nimport numpy as np\n\n#+end_src\n\n* Complexity\n- Time: \n- Space: \n\n* Related Concepts\n- \n"
         :target (file+head "permanent/ml/${slug}.org"
                            "#+title: ${title}\n#+filetags: :permanent:ml:\n#+created: %U\n\n")
         :unnarrowed t)
        
        ("pc" "Calculus" plain
         "* Definition\n%?\n\n* Formula\n\\[\n\n\\]\n\n* Intuition\n\n* Geometric Interpretation\n\n* Properties\n- \n\n* Applications in ML\n\n* Examples\n\n* Derivation\n"
         :target (file+head "permanent/math/calculus/${slug}.org"
                            "#+title: ${title}\n#+filetags: :permanent:math:calculus:\n#+created: %U\n\n")
         :unnarrowed t)
        
        ("pl" "Linear Algebra" plain
         "* Definition\n%?\n\n* Mathematical Notation\n\\[\n\n\\]\n\n* Geometric Interpretation\n\n* Properties\n- \n\n* Applications in ML\n\n* NumPy Implementation\n#+begin_src python\nimport numpy as np\n\n#+end_src\n\n* Related Concepts\n"
         :target (file+head "permanent/math/linear-algebra/${slug}.org"
                            "#+title: ${title}\n#+filetags: :permanent:math:linalg:\n#+created: %U\n\n")
         :unnarrowed t)
        
        ("ps" "Statistics/Probability" plain
         "* Definition\n%?\n\n* Formula\n\\[\n\n\\]\n\n* When to Use\n\n* Intuition\n\n* ML Applications\n\n* Code Example\n#+begin_src python\nimport numpy as np\nimport scipy.stats as stats\n\n#+end_src\n\n* Common Misconceptions\n"
         :target (file+head "permanent/math/statistics/${slug}.org"
                            "#+title: ${title}\n#+filetags: :permanent:math:stats:\n#+created: %U\n\n")
         :unnarrowed t)
        
        ("pd" "Data/SQL Concept" plain
         "* Concept\n%?\n\n* Theory\n\n* Syntax\n#+begin_src sql\n\n#+end_src\n\n* Use Cases\n\n* Performance Characteristics\n\n* Best Practices\n\n* Common Pitfalls\n\n* Examples\n#+begin_src sql\n-- Example 1\n\n-- Example 2\n\n#+end_src\n"
         :target (file+head "permanent/data/${slug}.org"
                            "#+title: ${title}\n#+filetags: :permanent:data:sql:\n#+created: %U\n\n")
         :unnarrowed t)
        
        ("pg" "GCP Service/Concept" plain
         "* Service Overview\n%?\n\n* Key Capabilities\n- \n\n* Architecture\n\n* Use Cases\n\n* Pricing Model\n\n* Best Practices\n\n* Integration Points\n\n* Comparison with Alternatives\n\n* Configuration\n#+begin_src bash\n\n#+end_src\n\n* Related Services\n"
         :target (file+head "permanent/gcp/${slug}.org"
                            "#+title: ${title}\n#+filetags: :permanent:gcp:\n#+created: %U\n\n")
         :unnarrowed t)
        
        ("pp" "Python Concept" plain
         "* Concept\n%?\n\n* Syntax\n#+begin_src python\n\n#+end_src\n\n* How It Works\n\n* Use Cases\n\n* Best Practices\n\n* Common Patterns\n#+begin_src python\n# Pattern 1\n\n# Pattern 2\n\n#+end_src\n\n* Pitfalls to Avoid\n\n* Performance Considerations\n\n* Related Concepts\n"
         :target (file+head "permanent/python/${slug}.org"
                            "#+title: ${title}\n#+filetags: :permanent:python:\n#+created: %U\n\n")
         :unnarrowed t)

        ("pa" "Advanced SQL" plain
 "* Concept\n%?\n\n* Theory\n\n* Syntax\n#+begin_src sql\n\n#+end_src\n\n* Query Examples\n#+begin_src sql\n-- Example 1\n\n-- Example 2\n\n#+end_src\n\n* Performance & Execution Plan\n\n* When to Use\n\n* Pitfalls\n\n* Related Concepts\n"
 :target (file+head "permanent/data/advanced-sql/${slug}.org"
                    "#+title: ${title}\n#+filetags: :permanent:data:sql:advanced:\n#+created: %U\n\n")
 :unnarrowed t)


        ;; ============ TOOLS ============
("t" "Tools")

("te" "Emacs" plain
 "* Concept\n%?\n\n* Keybindings\n\n* Configuration\n#+begin_src emacs-lisp\n\n#+end_src\n\n* Use Cases\n\n* Related Packages\n\n* Tips & Tricks\n"
 :target (file+head "tools/emacs/${slug}.org"
                    "#+title: ${title}\n#+filetags: :tools:emacs:\n#+created: %U\n\n")
 :unnarrowed t)
        

        ;; ============ Languages ============
("l" "Languages")

("lg" "German" plain
 "* Concept\n%?\n\n* Examples\n\n* Grammar Notes\n\n* Usage\n\n* Vocabulary\n\n* Related Concepts\n"
 :target (file+head "languages/german/${slug}.org"
                    "#+title: ${title}\n#+filetags: :language:german:\n#+created: %U\n\n")
 :unnarrowed t)

        ;; ============ PROJECTS ============
        ("j" "Project" plain
         "* Project Goal\n%?\n\n* Dataset/Data Source\n- Source: \n- Size: \n- Features: \n\n* Approach\n\n* Tech Stack\n- \n\n* Implementation\n#+begin_src python\n\n#+end_src\n\n* Results\n\n* Challenges & Solutions\n\n* Key Learnings\n\n* Improvements/Next Steps\n\n* Links to Concepts Used\n"
         :target (file+head "projects/${slug}.org"
                            "#+title: ${title}\n#+filetags: :project:\n#+created: %U\n\n")
         :unnarrowed t)
        
        ;; ============ RESOURCES ============
        ("r" "Resource" plain
         "* Resource Details\n- Type: %^{Type|Book|Course|Paper|Video|Article|Documentation}\n- Author/Creator: \n- URL/Location: %?\n- Status: %^{Status|To Read|Reading|Completed}\n\n* Summary\n\n* Key Takeaways\n- \n\n* Topics Covered\n\n* My Notes\n\n* Rating: /5\n\n* Quotes/Highlights\n\n* Action Items\n\n* Related Resources\n"
         :target (file+head "resources/${slug}.org"
                            "#+title: ${title}\n#+filetags: :resource:\n#+created: %U\n\n")
         :unnarrowed t)
        
        ;; ============ PEOPLE ============
        ("y" "Person/Researcher" plain
         "* Background\n%?\n\n* Known For\n- \n\n* Key Publications/Work\n\n* Contributions to Field\n\n* Relevant to My Study\n- \n\n* Resources\n- Papers: \n- Talks: \n- Website: \n\n* Notes\n"
         :target (file+head "people/${slug}.org"
                            "#+title: ${title}\n#+filetags: :people:\n#+created: %U\n\n")
         :unnarrowed t)
        
        ;; ============ DEFAULT ============
        ("d" "default" plain
         "* ${title}\n%?"
         :target (file+head "${slug}.org"
                            "#+title: ${title}\n#+created: %U\n\n")
         :unnarrowed t)
        ))
  
  ;; Daily notes templates
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %<%H:%M> %?"
           :target (file+head "%<%Y/%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d %A>\n#+filetags: :daily:\n\n* 📚 Learning Log\n\n* ✅ Completed\n\n* ❓ Questions\n\n* 📝 Tomorrow's Plan\n"))
          
          ("s" "Study Session" entry
           "* %<%H:%M> 📖 %^{Topic}\n:PROPERTIES:\n:DOMAIN: %^{Domain|ML|Math|Data|GCP|Python|Exam Prep}\n:DURATION: %^{Duration|30min|1hr|1.5hr|2hr}\n:FOCUS: %^{Focus|Theory|Practice|Project|Review}\n:END:\n\n** Learning Notes\n%?\n\n** Code/Examples\n\n** Questions/Blockers\n\n** Links\n- \n\n** Next Time\n"
           :target (file+head "%<%Y/%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d %A>\n#+filetags: :daily:\n\n"))
          
          ("p" "Practice Problem" entry
           "* %<%H:%M> 💪 %^{Problem Title}\n:PROPERTIES:\n:TYPE: %^{Type|Coding|Math|SQL|Concept}\n:DIFFICULTY: %^{Difficulty|Easy|Medium|Hard}\n:SOURCE: %^{Source|LeetCode|Kaggle|Book|Course}\n:TIME_SPENT: %^{Time}\n:STATUS: %^{Status|Solved|Partial|Stuck|Review}\n:END:\n\n** Problem Statement\n%?\n\n** My Approach\n\n** Solution\n#+begin_src python\n\n#+end_src\n\n** Learnings\n\n** Similar Problems\n"
           :target (file+head "%<%Y/%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d %A>\n#+filetags: :daily:\n\n"))
          
          ("x" "Exam Practice" entry
           "* %<%H:%M> 🎯 Exam Prep: %^{Exam|CDL|ACE|CDE|MLE}\n:PROPERTIES:\n:TOPIC: %^{Topic}\n:SCORE: %^{Score (if practice test)}\n:END:\n\n** What I Studied\n%?\n\n** Key Takeaways\n\n** Weak Areas\n\n** Action Items\n"
           :target (file+head "%<%Y/%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d %A>\n#+filetags: :daily:\n\n"))
          )))
 

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
