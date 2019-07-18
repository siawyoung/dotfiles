;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Lau Siaw Young"
      user-mail-address "lausiawyoung@gmail.com"
      epa-file-encrypt-to user-mail-address)

;;; When starting up, maximize the window
(when IS-MAC
  (add-hook 'window-setup-hook #'toggle-frame-maximized))

(when IS-MAC
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-copy-env "GOROOT"))

(global-visual-line-mode t)

(after! doom-modeline
  (setq doom-modeline-checker-simple-format nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq size-indication-mode nil)
  (setq mode-line-percent-position nil)
  (setq line-number-mode nil))

(after! company
  ;; disable automatic company suggestions, instead
  (setq company-idle-delay nil)
  ;; define custom keymapping for showing autocompletes
  (define-key evil-insert-state-map (kbd "C-c C-c") 'company-complete))

(after! flycheck
  (setq flycheck-check-syntax-automatically '(save)))

(def-package! lsp-python-ms
  :hook (python-mode . lsp))

(def-package! pyenv-mode
  :diminish
  :config
  (pyenv-mode))
(load! "+theming")
(load! "+navigation")
(load! "+vc")
(load! "+functions")
