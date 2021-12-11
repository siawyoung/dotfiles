;;; ~/.doom.d/theming.el -*- lexical-binding: t; -*-

(when IS-MAC
  (setq doom-font (font-spec :family "Fira Code" :size 16))
  (setq ns-use-thin-smoothing t))

(when IS-LINUX
  (setq doom-font (font-spec :family "Ubuntu Mono" :size 22)))

