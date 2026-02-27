;; keybindings.el
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c v"   "voice/whisper"
    "C-c v t" "transcribe to buffer"
    "C-c v d" "daily voice note"
    "C-c v q" "quick voice inbox"))


(provide 'keybindings)
;;; keybindings.el ends here
