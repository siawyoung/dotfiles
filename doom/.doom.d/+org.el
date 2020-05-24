;;; ~/dotfiles/doom/.doom.d/+org.el -*- lexical-binding: t; -*-

(setq org-startup-folded nil)
(setq org-catch-invisible-edits 'smart)
(setq org-log-done t)
(setq work-org (concat org-directory "/work/forge.org"))

(setq org-capture-templates
      '(("w" "Work" entry (file+headline work-org "JIRA") "** TODO %?\nADDED: %T\n\n%i\n\n" :prepend t)))

;; org-journal
(use-package! org-journal
  :config
  (setq org-journal-dir (concat org-directory "/journal/"))
  (setq org-journal-file-type `monthly)
  (setq org-journal-date-format "%A, %d %B %Y")
  (setq org-journal-file-format (concat (downcase (format-time-string "%Y/%b" (current-time))) ".org"))
  )

(map!
 "C-c j" #'org-journal-new-entry)

(setq org-image-actual-width '(600))

(use-package! org-projectile
  :config
  (setq org-projectile-projects-file (concat org-directory "work/"))
  (push (org-projectile-project-todo-entry) org-capture-templates)
  (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files))))

(map!
 "C-c n p" #'org-projectile-project-todo-completing-read
 "C-c n o" #'org-projectile-goto-location-for-project)

(add-to-list 'org-capture-templates
  '("t" "Personal todo" entry
   (file+headline +org-capture-todo-file "Inbox")
   "* TODO %?\n%i\n" :prepend t))

(use-package! org-roam
  :custom
  (org-roam-directory org-directory))
