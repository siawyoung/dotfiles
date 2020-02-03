;;; ~/.doom.d/theming.el -*- lexical-binding: t; -*-

(when IS-MAC
  (setq doom-font (font-spec :family "SF Mono" :size 16))
  (setq ns-use-thin-smoothing t))

(load-theme 'doom-vibrant t)

;; highlight active buffer
(solaire-global-mode +1)
(add-hook 'after-revert-hook #'turn-on-solaire-mode)
(add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
(solaire-mode-swap-bg)
