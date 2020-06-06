;;; ~/github/dotfiles/.doom.d/+functions.el -*- lexical-binding: t; -*-

(defun sy/minify-buffer-contents()
  "Minifies the buffer contents by removing whitespaces. Most commonly used for JSON."
  (interactive)
  (delete-whitespace-rectangle (point-min) (point-max))
  (mark-whole-buffer)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "" nil t)))

(defun unfill-region (start end)
  "Replace newline chars in region by single spaces.
This command does the reverse of `fill-region'."
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region start end)))
