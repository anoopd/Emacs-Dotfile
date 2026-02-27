;; ============================================
;; WHISPER.CPP SETUP - OPTIMIZED FOR YOUR HARDWARE
;; ============================================

(defvar my/whisper-cpp-path "~/whisper.cpp/build/bin/whisper-cli"
  "Path to whisper.cpp executable")

(defvar my/whisper-cpp-model "~/whisper.cpp/models/ggml-base.bin"
  "Path to whisper.cpp model (base = good balance)")

(defun my/whisper-transcribe (audio-file)
  "Transcribe audio file using whisper.cli."
  (let ((output (shell-command-to-string
                 (format "%s -m %s -f %s -nt -l en -t 4 2>/dev/null"  ; -t 4 = use 4 CPU threads
                         my/whisper-cpp-path
                         my/whisper-cpp-model
                         audio-file))))
    (string-trim output)))

(defun my/whisper-to-org ()
  "Record audio and transcribe to current buffer."
  (interactive)
  (let ((audio-file (make-temp-file "whisper-" nil ".wav")))
    
    ;; Record audio
    (message "🎤 Recording... Press C-g when done")
    (condition-case nil
        (call-process "sox" nil nil nil 
                     "-d" "-r" "16000" "-c" "1" audio-file)
      (quit (message "Recording stopped")))
    
    ;; Check if file has content
    (if (> (file-attribute-size (file-attributes audio-file)) 1000)
        (progn
          (message "⚡ Transcribing...")
          (let ((transcript (my/whisper-transcribe audio-file)))
            (if (> (length transcript) 0)
                (progn
                  (insert transcript)
                  (message "✅ Transcription complete!"))
              (message "❌ No speech detected"))))
      (message "❌ Recording too short"))
    
    ;; Cleanup
    (delete-file audio-file)))

(global-set-key (kbd "C-c v t") 'my/whisper-to-org)

;; ============================================
;; VOICE CAPTURE FOR ORG-ROAM
;; ============================================

(defun my/whisper-to-roam-daily ()
  "Add voice note to today's daily note."
  (interactive)
  (let ((audio-file (make-temp-file "whisper-daily-" nil ".wav")))
    
    (message "🎤 Recording daily note... Press C-g when done")
    (condition-case nil
        (call-process "sox" nil nil nil 
                     "-d" "-r" "16000" "-c" "1" audio-file)
      (quit nil))
    
    (when (> (file-attribute-size (file-attributes audio-file)) 1000)
      (message "⚡ Transcribing...")
      (let ((transcript (my/whisper-transcribe audio-file)))
        ;; Go to today's daily note
        (org-roam-dailies-goto-today)
        (goto-char (point-max))
        (insert (format "\n* %s Voice Note\n%s\n" 
                       (format-time-string "%H:%M")
                       transcript))
        (message "✅ Added to daily note!")))
    
    (delete-file audio-file)))

(global-set-key (kbd "C-c v d") 'my/whisper-to-roam-daily)

;; ============================================
;; QUICK VOICE CAPTURE WITH AUTO-STRUCTURING
;; ============================================

(defun my/whisper-quick-note ()
  "Voice note with automatic categorization."
  (interactive)
  (let ((audio-file (make-temp-file "whisper-quick-" nil ".wav")))
    
    (message "🎤 Recording... Press C-g when done")
    (condition-case nil
        (call-process "sox" nil nil nil 
                     "-d" "-r" "16000" "-c" "1" audio-file)
      (quit nil))
    
    (when (> (file-attribute-size (file-attributes audio-file)) 1000)
      (message "⚡ Processing...")
      (let ((transcript (my/whisper-transcribe audio-file))
            (timestamp (format-time-string "%Y-%m-%d %H:%M")))
        
        ;; Create a simple capture entry
        (with-current-buffer (find-file-noselect 
                             (expand-file-name "voice-inbox.org" org-roam-directory))
          (goto-char (point-max))
          (insert (format "\n* Voice Note - %s\n:PROPERTIES:\n:CREATED: [%s]\n:END:\n\n%s\n\n** TODO Process this note\n"
                         timestamp
                         timestamp
                         transcript))
          (save-buffer))
        
        (message "✅ Saved to voice-inbox.org")))
    
    (delete-file audio-file)))

(global-set-key (kbd "C-c v q") 'my/whisper-quick-note)


;; Add to whisper.el or separate voice-commands.el

(defun my/whisper-org-formatter ()
  "Format whisper dictation for org-mode."
  (interactive)
  (let ((case-fold-search t))  ; case-insensitive
    (save-excursion
      ;; Headings
      (goto-char (point-min))
      (while (re-search-forward "new heading \\([^.
]+\\)" nil t)
        (replace-match "\n* \\1\n"))
      
      (goto-char (point-min))
      (while (re-search-forward "sub heading \\([^.
]+\\)" nil t)
        (replace-match "\n** \\1\n"))
      
      ;; Lists
      (goto-char (point-min))
      (while (re-search-forward "new item \\([^.
]+\\)" nil t)
        (replace-match "\n- \\1"))
      
      ;; Cleanup
      (goto-char (point-min))
      (while (re-search-forward "\n\n\n+" nil t)
        (replace-match "\n\n")))))

;; Bind to a key for manual cleanup
(global-set-key (kbd "C-c v f") #'my/whisper-org-formatter)

;; Or auto-run after whisper (if whisper.el supports hooks)
;; Check whisper.el for available hooks

;; ============================================
;; HELPER: Check if whisper.cpp is set up
;; ============================================

(defun my/check-whisper-setup ()
  "Check if whisper.cpp is properly configured."
  (interactive)
  (cond
   ((not (file-exists-p my/whisper-cpp-path))
    (message "❌ whisper.cpp not found at %s" my/whisper-cpp-path))
   ((not (file-exists-p my/whisper-cpp-model))
    (message "❌ Model not found at %s" my/whisper-cpp-model))
   ((not (executable-find "sox"))
    (message "❌ sox not installed. Run: sudo apt install sox"))
   (t
    (message "✅ Whisper setup looks good!"))))

;; Run on startup
(run-with-idle-timer 2 nil 'my/check-whisper-setup)

(provide 'whisper)
