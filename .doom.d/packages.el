;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

(package! diminish)
(package! exec-path-from-shell)
(package! forge)
(package! git-link)
(package! persistent-scratch)
(package! deadgrep)
(package! key-chord)

;;; language/syntax-specific packages
(package! json-mode)
(package! protobuf-mode)

;; :lang/go
(package! go-playground)

;; :lang/javascript
(package! eslintd-fix)
(package! add-node-modules-path)
(package! prettier-js)

;; :lang/python
(package! lsp-python-ms)
(package! pyenv-mode)
(package! python-black)
