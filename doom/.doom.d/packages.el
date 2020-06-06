;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

(package! diminish)
(package! exec-path-from-shell)
(package! forge)
(package! git-link)
(package! persistent-scratch)
(package! deadgrep)
(package! key-chord)
(package! evil-mc)
(package! git-auto-commit-mode)

;;; language/syntax-specific packages
(package! json-mode)
(package! protobuf-mode)

;; :lang/go
;; (package! go-playground)

;; :lang/html
(package! emmet-mode)

;; :lang/javascript
(package! add-node-modules-path)
(package! prettier-js)
;; remove stuff from doom's js layer that i don't need
(package! coffee-mode :disable t)
(package! js2-refactor :disable t)
(package! nodejs-repl :disable t)
(package! vue-mode)

;; :lang/python
(package! lsp-python-ms)
(package! pyenv-mode)
(package! python-black)

;; :lang/scala
(package! ensime :disable t)

;; org
(package! org-journal)
(package! org-projectile)

;; :lang/graphql
(package! graphql-mode)
