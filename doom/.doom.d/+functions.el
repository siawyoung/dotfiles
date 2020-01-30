;;; ~/github/dotfiles/.doom.d/+functions.el -*- lexical-binding: t; -*-

(defun sy/minify-buffer-contents()
  "Minifies the buffer contents by removing whitespaces. Most commonly used for JSON."
  (interactive)
  (delete-whitespace-rectangle (point-min) (point-max))
  (mark-whole-buffer)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "" nil t)))
