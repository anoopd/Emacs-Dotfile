;;; simple-clock-config.el --- Simple Time Tracking with org-clock -*- lexical-binding: t -*-

;;; Commentary:
;; Time tracking using only built-in org-clock (no external packages).
;; Tracks study/work hours, generates reports, logs to daily notes.
;; Loaded from init.el via (require 'simple-clock-config)

;;; Code:

;; ============================================================================
;; ORG-CLOCK SETTINGS
;; ============================================================================

(with-eval-after-load 'org
  (setq org-clock-persist          'history
        org-clock-in-resume        t
        org-clock-persist-query-resume nil
        org-clock-out-remove-zero-time-clocks t
        org-clock-report-include-clocking-task t
        org-clock-idle-time         10)  ; prompt if idle for 10 min

  ;; Persist clock history across Emacs sessions
  (org-clock-persistence-insinuate)

  ;; Show current clocked task in mode-line
  (setq org-clock-mode-line-total 'current))

;; ============================================================================
;; CLOCK HELPERS
;; ============================================================================

(defun my/clock-in-last ()
  "Clock in to the last clocked task."
  (interactive)
  (org-clock-in '(4)))

(defun my/clock-report-today ()
  "Show clock report for today only."
  (interactive)
  (let ((org-clock-report-include-clocking-task t))
    (org-clock-report)))

(defun my/total-clocked-today ()
  "Show total time clocked today across all tasks."
  (interactive)
  (let* ((today-start (format-time-string "%Y-%m-%d 00:00:00"))
         (today-end   (format-time-string "%Y-%m-%d 23:59:59"))
         (total 0))
    (org-map-entries
     (lambda ()
       (let ((clocked (org-clock-sum (time-to-seconds (date-to-time today-start))
                                     (time-to-seconds (date-to-time today-end)))))
         (setq total (+ total (or (org-clock-sum-current-item) 0)))))
     t
     'agenda)
    (message "Total clocked today: %s"
             (org-duration-from-minutes (/ total 60)))))

;; ============================================================================
;; STUDY/WORK TIMER (Simple 25-min timer without external package)
;; ============================================================================

(defvar my/study-timer nil
  "Timer object for study/work sessions.")

(defun my/start-study-timer (minutes)
  "Start a simple countdown timer for MINUTES and clock in."
  (interactive "nMinutes: ")
  (when my/study-timer
    (cancel-timer my/study-timer))
  
  ;; Clock in if on an org heading
  (when (and (derived-mode-p 'org-mode)
             (org-at-heading-p))
    (org-clock-in))
  
  (setq my/study-timer
        (run-at-time (* minutes 60) nil
                     (lambda ()
                       (message "⏰ Timer finished! (%d minutes)" minutes)
                       (setq my/study-timer nil))))
  (message "🕐 Timer started: %d minutes" minutes))

(defun my/stop-timer ()
  "Stop the current timer and clock out."
  (interactive)
  (when my/study-timer
    (cancel-timer my/study-timer)
    (setq my/study-timer nil))
  (when (org-clock-is-active)
    (org-clock-out))
  (message "Timer stopped and clocked out."))

;; Quick presets
(defun my/study-25min ()
  "Start a 25-minute study session."
  (interactive)
  (my/start-study-timer 25))

(defun my/study-50min ()
  "Start a 50-minute study session."
  (interactive)
  (my/start-study-timer 50))

;; ============================================================================
;; KEYBINDINGS
;; ============================================================================

(defun my/clock-keybindings-setup ()
  "Register clock keybindings after Evil is loaded."
  (when (featurep 'evil)
    (evil-define-key 'normal 'global
      ;; Basic clocking
      (kbd "SPC p i") #'org-clock-in
      (kbd "SPC p o") #'org-clock-out
      (kbd "SPC p g") #'org-clock-goto
      (kbd "SPC p l") #'my/clock-in-last
      
      ;; Reports
      (kbd "SPC p r") #'org-clock-report
      (kbd "SPC p t") #'my/total-clocked-today
      
      ;; Simple timers
      (kbd "SPC p 1") #'my/study-25min   ; 25-min session
      (kbd "SPC p 2") #'my/study-50min   ; 50-min session
      (kbd "SPC p s") #'my/stop-timer)
    
    (evil-normalize-keymaps)))

(add-hook 'after-init-hook #'my/clock-keybindings-setup)

(provide 'simple-clock-config)
;;; simple-clock-config.el ends here
