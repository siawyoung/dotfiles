;;; ~/github/dotfiles/.doom.d/+vc.el -*- lexical-binding: t; -*-

(after! projectile
    ;; https://emacs.stackexchange.com/questions/32634/how-can-the-list-of-projects-used-by-projectile-be-manually-updated/3
  (when (require 'magit nil t)
    (mapc #'projectile-add-known-project
          (mapcar #'file-name-as-directory (magit-list-repos)))
    ;; Optionally persist
    (projectile-save-known-projects)))

(after! magit
  (setq magit-branch-prefer-remote-upstream t)
  ;; find all git projects using magit
  (setq magit-repository-directories
        '(("~/github/" . 1)
          ("~/github/go/src/github.com/carousell/" . 1))))

;; reload buffers if changes happen in buffers due to git
(global-auto-revert-mode 1)
