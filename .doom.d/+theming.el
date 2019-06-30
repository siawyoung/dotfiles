;;; ~/.doom.d/theming.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "SF Mono" :size 16))

(when IS-LINUX
  (font-put doom-font :weight 'semi-light))
(when IS-MAC
  (setq ns-use-thin-smoothing t))
