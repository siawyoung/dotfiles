;;; ~/.doom.d/+navigation.el -*- lexical-binding: t; -*-

(map! :n "C-h" #'evil-window-left
      :n "C-j" #'evil-window-down
      :n "C-k" #'evil-window-up
      :n "C-l" #'evil-window-right

      :n "C-x h" #'previous-buffer
      :n "C-x l" #'next-buffer)

(after! evil-escape
  (setq evil-escape-unordered-key-sequence t))
