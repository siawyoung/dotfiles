;;; ~/.doom.d/+navigation.el -*- lexical-binding: t; -*-

(map! :n "C-h" #'evil-window-left
      :n "C-j" #'evil-window-down
      :n "C-k" #'evil-window-up
      :n "C-l" #'evil-window-right

      :n "C-x h" #'previous-buffer
      :n "C-x l" #'next-buffer)

(after! evil-escape
  (setq evil-escape-unordered-key-sequence t))

(after! evil
  (evil-define-key 'visual evil-mc-key-map
  "A" #'evil-mc-make-cursor-in-visual-selection-end
  "I" #'evil-mc-make-cursor-in-visual-selection-beg))
