;;; org-agenda-config.el --- ML Roadmap Org-Agenda Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Org-agenda setup for ML learning roadmap.
;; Loaded from init.el via (require 'org-agenda-config)

;;; Code:

;; ============================================================================
;; TODO KEYWORDS
;; ============================================================================

(with-eval-after-load 'org
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i!)" "WAITING(w@)" "|" "DONE(d!)" "CANCELLED(c@)")))

  (setq org-todo-keyword-faces
        '(("TODO"        . (:foreground "#fb4934" :weight bold))
          ("IN-PROGRESS" . (:foreground "#fabd2f" :weight bold))
          ("WAITING"     . (:foreground "#83a598" :weight bold))
          ("DONE"        . (:foreground "#b8bb26" :weight bold))
          ("CANCELLED"   . (:foreground "#928374" :strike-through t)))))

;; ============================================================================
;; AGENDA FILES
;; ============================================================================

(with-eval-after-load 'org-agenda
  (setq org-agenda-files
        (list
         (expand-file-name "journey.org"   "~/org-roam/")
         (expand-file-name "inbox.org"     "~/org-roam/")
         (expand-file-name "roam-dailies/" "~/org-roam/")))

;; ============================================================================
;; AGENDA APPEARANCE
;; ============================================================================

  (setq org-agenda-block-separator     ?─
        org-agenda-compact-blocks       t
        org-agenda-start-with-log-mode  nil
        org-agenda-log-mode-items       '(closed clock)
        org-agenda-window-setup         'current-window
        org-agenda-span                 7
        org-agenda-start-on-weekday     1
        org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))

;; ============================================================================
;; CUSTOM AGENDA COMMANDS
;; ============================================================================

  (setq org-agenda-custom-commands
        '(
          ;; ------------------------------------------------------------------
          ;; m — Full ML Roadmap Dashboard
          ;; ------------------------------------------------------------------
          ("m" "ML Roadmap Dashboard"
           ((agenda ""
                    ((org-agenda-span 7)
                     (org-agenda-start-on-weekday 1)
                     (org-agenda-overriding-header "── This Week ───────────────────────────")))

            (todo "IN-PROGRESS"
                  ((org-agenda-overriding-header "── Currently Studying ──────────────────")
                   (org-agenda-files
                    (list (expand-file-name "journey.org" "~/org-roam/")))))

            (todo "TODO"
                  ((org-agenda-overriding-header "── Up Next (Top 5) ─────────────────────")
                   (org-agenda-max-entries 5)
                   (org-agenda-files
                    (list (expand-file-name "journey.org" "~/org-roam/")))))

            (todo "WAITING"
                  ((org-agenda-overriding-header "── Blocked / Waiting ───────────────────")
                   (org-agenda-files
                    (list (expand-file-name "journey.org" "~/org-roam/")))))

            (tags "DEADLINE<=\"<+30d>\"+TODO=\"TODO\""
                  ((org-agenda-overriding-header "── Deadlines in 30 Days ────────────────")
                   (org-agenda-files
                    (list (expand-file-name "journey.org" "~/org-roam/"))))))
           nil)

          ;; ------------------------------------------------------------------
          ;; d — Today's Study Plan
          ;; ------------------------------------------------------------------
          ("d" "Today's Study Plan"
           ((agenda ""
                    ((org-agenda-span 1)
                     (org-agenda-overriding-header "── Today's Schedule ────────────────────")))

            (tags-todo "ml|math|python|gcp|data|exam"
                       ((org-agenda-overriding-header "── Active ML Tasks ─────────────────────")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'todo '("DONE" "CANCELLED"))))))
           nil)

          ;; ------------------------------------------------------------------
          ;; e — Exam Prep View
          ;; ------------------------------------------------------------------
          ("e" "Exam Prep"
           ((todo "TODO"
                  ((org-agenda-overriding-header "── CDL ─────────────────────────────────")
                   (org-agenda-files
                    (list (expand-file-name "exams/cdl/" "~/org-roam/")))))

            (todo "TODO"
                  ((org-agenda-overriding-header "── ACE ─────────────────────────────────")
                   (org-agenda-files
                    (list (expand-file-name "exams/ace/" "~/org-roam/")))))

            (todo "TODO"
                  ((org-agenda-overriding-header "── CDE ─────────────────────────────────")
                   (org-agenda-files
                    (list (expand-file-name "exams/cde/" "~/org-roam/")))))

            (todo "TODO"
                  ((org-agenda-overriding-header "── MLE ─────────────────────────────────")
                   (org-agenda-files
                    (list (expand-file-name "exams/mle/" "~/org-roam/"))))))
           nil)

          ;; ------------------------------------------------------------------
          ;; w — Weekly Review
          ;; ------------------------------------------------------------------
          ("w" "Weekly Review"
           ((agenda ""
                    ((org-agenda-span 7)
                     (org-agenda-start-day "-7d")
                     (org-agenda-overriding-header "── Last 7 Days ─────────────────────────")))

            (todo "DONE"
                  ((org-agenda-overriding-header "── Completed This Week ─────────────────")))

            (todo "TODO"
                  ((org-agenda-overriding-header "── Still TODO ──────────────────────────"))))
           nil))))

;; ============================================================================
;; KEYBINDINGS
;;
;; Root cause of the original bug:
;;   Your init.el does (evil-define-key 'normal 'global (kbd "SPC") nil)
;;   which wipes the entire SPC prefix map. Any SPC a * bindings defined
;;   before or during init.el evaluation get erased by that line.
;;
;; Fix: use after-init-hook so this runs AFTER all of init.el has finished,
;;   including elpaca installing evil, evil-mode enabling, and the SPC nil
;;   assignment. We are the last thing to run, so nothing overwrites us.
;; ============================================================================

(defun my/agenda-keybindings-setup ()
  "Register SPC a * keybindings after Evil and init are fully loaded."
  (when (featurep 'evil)
    (evil-define-key 'normal 'global
      ;; Agenda dispatcher
      (kbd "SPC a a") #'org-agenda

      ;; Direct view shortcuts
      (kbd "SPC a m") (lambda () (interactive) (org-agenda nil "m"))
      (kbd "SPC a d") (lambda () (interactive) (org-agenda nil "d"))
      (kbd "SPC a e") (lambda () (interactive) (org-agenda nil "e"))
      (kbd "SPC a w") (lambda () (interactive) (org-agenda nil "w"))

      ;; Task management (works in any org buffer)
      (kbd "SPC a t") #'org-todo
      (kbd "SPC a s") #'org-schedule
      (kbd "SPC a D") #'org-deadline)

    ;; Force evil to recognise the new bindings immediately
    (evil-normalize-keymaps)))

(add-hook 'after-init-hook #'my/agenda-keybindings-setup)

(provide 'org-agenda-config)
;;; org-agenda-config.el ends here;
