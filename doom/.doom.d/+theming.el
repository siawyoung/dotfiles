;;; ~/.doom.d/theming.el -*- lexical-binding: t; -*-

(when IS-MAC
  (setq doom-font (font-spec :family "Fira Code" :size 16))
  (setq ns-use-thin-smoothing t))

(when IS-LINUX
  (setq doom-font (font-spec :family "Ubuntu Mono" :size 22)))

;; highlight active buffer
(solaire-global-mode +1)
(add-hook 'after-revert-hook #'turn-on-solaire-mode)
(add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
(solaire-mode-swap-bg)
