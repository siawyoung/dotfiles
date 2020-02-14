;;; ~/dotfiles/doom/.doom.d/+org.el -*- lexical-binding: t; -*-

(setq org-startup-folded nil)
(setq org-catch-invisible-edits 'smart)

;; org-journal
(use-package! org-journal
  :config
  (setq org-journal-dir (concat org-directory "journal/"))
  (setq org-journal-file-type `monthly)
  (setq org-journal-date-format "%A, %d %B %Y")
  (setq org-journal-file-format (concat (downcase (format-time-string "%Y/%b" (current-time))) ".org"))
  )

(map!
 "C-c j" #'org-journal-new-entry)

(setq org-image-actual-width '(600))
