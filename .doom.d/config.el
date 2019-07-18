;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Lau Siaw Young"
      user-mail-address "sy@siawyoung.com"
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
  ;; only flycheck on save, disable all other events
  (setq flycheck-check-syntax-automatically '(save)))

(def-package! lsp-python-ms
  :hook (python-mode . lsp))

(def-package! pyenv-mode
  :diminish
  :config
  (pyenv-mode))

(def-package! js2-mode
  :config
  (setq js2-highlight-level 3)
  (setq js2-mode-show-parse-errors t)
  (setq js2-mode-show-strict-warnings nil)
  (setq js2-strict-missing-semi-warning nil)
  (setq flycheck-javascript-eslint-executable "eslint_d"))

(def-package! eslintd-fix)

(def-package! prettier-js
  :hook ((js2-mode . prettier-js-mode)
         (rjsx-mode . prettier-js-mode)))

(use-package add-node-modules-path
  :hook ((js2-mode . add-node-modules-path)
         (rjsx-mode . add-node-modules-path)))

(after! forge
  ;; I have way too many unread github notifications, running this will hang Emacs for days
  (setq forge-pull-notifications nil))

(after! persistent-scratch
  (persistent-scratch-setup-default)
  (setq persistent-scratch-autosave-mode 1))

(add-hook 'go-mode-hook (lambda()
                          (add-hook 'before-save-hook #'gofmt-before-save)
                          (setq gofmt-command "goimports")))

(load! "+theming")
(load! "+navigation")
(load! "+vc")
(load! "+functions")
