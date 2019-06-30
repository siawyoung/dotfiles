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

(after! company
  (setq company-idle-delay 0.2))

(def-package! lsp-python-ms
  :hook (python-mode . lsp))

(load! "+theming")
(load! "+navigation")
(load! "+vc")
(load! "+functions")
