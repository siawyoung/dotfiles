;;; ~/github/dotfiles/.doom.d/+functions.el -*- lexical-binding: t; -*-

(defun sy/minify-buffer-contents()
  "Minifies the buffer contents by removing whitespaces. Most commonly used for JSON."
  (interactive)
  (delete-whitespace-rectangle (point-min) (point-max))
  (mark-whole-buffer)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "" nil t)))

(defun sy/search-vterm()
  (interactive)
  (counsel-ibuffer)
  (ibuffer-filter-disable)
  (ibuffer-filter-by-mode 'vterm-mode))

(map! "C-c C-v" #'sy/search-vterm)

;; (defun sy/test()
;;   (interactive)
;;   (org-journal-new-entry nil)
;;   (insert ))

;; (minibuffer-with-setup-hook
;;     (lambda () (insert "BOX DRAWING"))
;;   (call-interactively 'insert-char))
