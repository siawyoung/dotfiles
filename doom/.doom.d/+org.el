;;; ~/dotfiles/doom/.doom.d/+org.el -*- lexical-binding: t; -*-

(setq org-directory "~/code/org")

;; org-journal
(use-package! org-journal
  :config
  (setq org-journal-dir "~/code/org/journal")
  (setq org-journal-file-type `monthly)
  (setq org-journal-date-format "%A, %d %B %Y")
  (setq org-journal-file-format (concat (downcase (format-time-string "%Y/%b" (current-time))) ".org"))
  )
