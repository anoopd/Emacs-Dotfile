;;; pomodoro-config.el --- Org-Pomodoro Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Pomodoro timer integrated with org-agenda and org-clock.
;; - Start a pomodoro directly from an agenda task
;; - Auto-clocks time into the org heading
;; - Desktop notifications on WSL2 via PowerShell
;; - Integrates with your habit targets (Study 4hr, Work 3hr)
;; Loaded from init.el via (require 'pomodoro-config)

;;; Code:

;; ============================================================================
;; ORG-POMODORO
;; ============================================================================

;; Install org-pomodoro explicitly via elpaca first
(elpaca (org-pomodoro :host github :repo "marcinkozikowski/org-pomodoro"))
(elpaca-wait)

(use-package org-pomodoro
  :after org-agenda

  :custom
  (org-pomodoro-length           25)   ; work interval in minutes
  (org-pomodoro-short-break-length 5)  ; short break
  (org-pomodoro-long-break-length 20)  ; long break after 4 pomodoros
  (org-pomodoro-long-break-frequency 4)

  ;; Audio alerts — set to nil if you don't want sound
  (org-pomodoro-play-sounds nil)

  ;; Format shown in modeline
  (org-pomodoro-format         "🍅 %s")
  (org-pomodoro-short-break-format "☕ %s")
  (org-pomodoro-long-break-format  "🛋️  %s")

  :config

  ;; ── WSL2 desktop notifications via PowerShell ──────────────────────────
  ;; Sends a Windows toast notification when a pomodoro ends

  (defun my/wsl-notify (title message)
    "Send a Windows toast notification from WSL2."
    (call-process "powershell.exe" nil nil nil
                  "-Command"
                  (format
                   "[Windows.UI.Notifications.ToastNotificationManager, Windows.UI.Notifications, ContentType=WindowsRuntime] | Out-Null; $template = [Windows.UI.Notifications.ToastNotificationManager]::GetTemplateContent([Windows.UI.Notifications.ToastTemplateType]::ToastText02); $template.SelectSingleNode('//text[@id=1]').InnerText = '%s'; $template.SelectSingleNode('//text[@id=2]').InnerText = '%s'; $toast = [Windows.UI.Notifications.ToastNotification]::new($template); [Windows.UI.Notifications.ToastNotificationManager]::CreateToastNotifier('Emacs').Show($toast);"
                   title message)))

  ;; Hook into pomodoro state changes
  (add-hook 'org-pomodoro-finished-hook
            (lambda ()
              (my/wsl-notify "🍅 Pomodoro Done!" "Time for a short break.")))

  (add-hook 'org-pomodoro-break-finished-hook
            (lambda ()
              (my/wsl-notify "☕ Break Over" "Back to work!")))

  (add-hook 'org-pomodoro-long-break-finished-hook
            (lambda ()
              (my/wsl-notify "🛋️  Long Break Over" "Ready for the next session?")))

  (add-hook 'org-pomodoro-killed-hook
            (lambda ()
              (my/wsl-notify "🍅 Pomodoro Killed" "Session ended early.")))

  ;; ── Auto-log to daily roam note ────────────────────────────────────────
  ;; Appends a line to today's daily note when a pomodoro finishes

  (defun my/pomodoro-log-to-daily ()
    "Log completed pomodoro to today's org-roam daily note."
    (when (org-pomodoro-active-p)
      (let* ((task  (org-clock-get-clock-string))
             (time  (format-time-string "%H:%M"))
             (entry (format "\n- 🍅 [%s] Pomodoro completed: %s" time task)))
        (save-window-excursion
          (org-roam-dailies-goto-today)
          (goto-char (point-max))
          (insert entry)
          (save-buffer)))))

  (add-hook 'org-pomodoro-finished-hook #'my/pomodoro-log-to-daily))

;; ============================================================================
;; ORG-CLOCK SETTINGS (pairs well with org-pomodoro)
;; ============================================================================

(with-eval-after-load 'org-clock
  (setq org-clock-persist          'history
        org-clock-in-resume        t
        org-clock-persist-query    t
        org-clock-report-include-clocking-task t)
  (org-clock-persistence-insinuate))

;; ============================================================================
;; KEYBINDINGS
;; ============================================================================

(defun my/pomodoro-keybindings-setup ()
  "Register pomodoro keybindings after Evil is loaded."
  (when (featurep 'evil)
    (evil-define-key 'normal 'global
      ;; Start pomodoro on task at point (use from agenda or org buffer)
      (kbd "SPC p p") #'org-pomodoro

      ;; Clock commands
      (kbd "SPC p i") #'org-clock-in
      (kbd "SPC p o") #'org-clock-out
      (kbd "SPC p r") #'org-clock-report     ; insert clock report table
      (kbd "SPC p g") #'org-clock-goto       ; jump to currently clocked task

      ;; Quick pomodoro status
      (kbd "SPC p s") (lambda ()
                        (interactive)
                        (if (org-pomodoro-active-p)
                            (message "🍅 Pomodoro running — %s remaining"
                                     (org-pomodoro-format-seconds))
                          (message "No active pomodoro. Start one with SPC p p"))))
    (evil-normalize-keymaps)))

(add-hook 'after-init-hook #'my/pomodoro-keybindings-setup)

(provide 'pomodoro-config)
;;; pomodoro-config.el ends here
