;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Lau Siaw Young"
      user-mail-address "lausiawyoung@gmail.com"
      epa-file-encrypt-to user-mail-address)

;;; When starting up, maximize the window
(when IS-MAC
  (add-hook 'window-setup-hook #'toggle-frame-maximized))

(load! "+theming")
(load! "+navigation")
(load! "+vc")
(load! "+functions")
