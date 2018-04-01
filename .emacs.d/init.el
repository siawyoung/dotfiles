;;; init.el --- This is my Emacs configuration

;; whoami
(setq user-full-name "Lau Siaw Young"
      user-mail-address "lausiawyoung@gmail.com")

;; load customize config
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; add package archives
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'delight)
  (package-install 'use-package))

(eval-and-compile
  (defvar use-package-verbose t)
  (require 'use-package)
  (require 'bind-key)
  (require 'delight)
  (require 'diminish)
  (setq use-package-always-ensure t))

;; run recentf every 5 minutes
(require 'recentf)
(run-at-time (* 5 60) nil
             (lambda ()
               (let ((inhibit-message t))
                 (recentf-save-list))))

;; validate config
;; check for emacs config errors
(use-package validate)

;; delete-selection mode to overwrite selection
(delete-selection-mode 1)

;; Use ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; prefer y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; load $PATH env variable into emacs
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

;; set universal font size to 17px
(set-face-attribute 'default nil :height 170)

;; Move all backup files to the OS's temp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Remove useless toolbars and splash screens
(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; Use single spaces
(setq sentence-end-double-space nil)

;; Use 2 spaces for tabs
(setq-default tab-width 2)
(setq-default js-indent-level 2)
(setq-default indent-tabs-mode nil)

;; delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; stop blinking cursor
(blink-cursor-mode 0)

;; custom function for reloading config
(defun reload-init ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; eshell
(require 'eshell)
(require 'em-smart)

;; org-mode
(require 'org)
;; soft-wrap lines
(setq org-startup-truncated nil)

;; don't garbage collect when minibuffer is open
;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;;;
;; Third party package config
;;;

;; load gruvbox
(load-theme 'gruvbox t)

(use-package centered-cursor-mode
  :diminish centered-cursor-mode
  :init
  (setq global-centered-cursor-mode 1))

;; smart-mode-line
(use-package smart-mode-line
  :init
  (add-hook 'after-init-hook 'sml/setup)
  :config
  (setq sml/theme 'respectful)
  (setq sml/shorten-directory t)
  (setq sml/position-percentage-format nil)
  ;; remove these stuff from the mode-line
  (setq rm-blacklist
        (format "^ \\(%s\\)$"
                (mapconcat #'identity
                           '("ElDoc")
                           "\\|")))
  ;; hide "~/github" prefix in mode-line
  (add-to-list 'sml/replacer-regexp-list '("^~/github" "")))

;; counsel config
(use-package counsel
  :diminish ivy-mode
  :bind
  (("C-c C-r" . ivy-resume)
   ("M-x" . counsel-M-x)
   ("C-s" . swiper)
   ("C-c i" . counsel-imenu)
   ("C-x C-f" . counsel-find-file)
   ("C-x j" . counsel-dired-jump)
   ("C-x l" . counsel-locate)
   ("C-c j" . counsel-git)
   ("C-c s" . counsel-projectile-rg)
   ("C-c f" . counsel-recentf)
   ("M-y" . counsel-yank-pop)
   :map swiper-map
   ("C-r" . ivy-previous-line)
   :map help-map
   ("f" . counsel-describe-function)
   ("v" . counsel-describe-variable)
   ("l" . counsel-info-lookup-symbol)
   :map ivy-minibuffer-map
   ("C-d" . ivy-dired)
   ("C-o" . ivy-occur)
   ("<return>" . ivy-alt-done)
   ("M-<return>" . ivy-immediate-done)
   :map read-expression-map
   ("C-r" . counsel-expression-history))
  :init
  (add-hook 'after-init-hook 'ivy-mode)
  :config
  (defun ivy-dired ()
    (interactive)
    (if ivy--directory
        (ivy-quit-and-run
         (dired ivy--directory)
         (when (re-search-forward
                (regexp-quote
                 (substring ivy--current 0 -1)) nil t)
           (goto-char (match-beginning 0))))
      (user-error
       "Not completing files currently")))
  (setq counsel-find-file-at-point t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((ivy-switch-buffer . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (ivy-set-actions
   t
   '(("I" insert "insert"))))

;; flx
(use-package flx)

;; magit config
(use-package magit
  :bind (("s-g" . magit-status)))

;; reload buffers if changes happen in buffers due to git
(diminish 'auto-revert-mode)
(global-auto-revert-mode 1)

(use-package perspective
  :init (persp-mode))

;; projectile config
(use-package projectile
  ;; show only the project name in mode line
  :delight '(:eval (concat " " (projectile-project-name)))
  :init
  (add-hook 'after-init-hook 'projectile-mode)
  :config
  (setq projectile-enable-caching t)
  (use-package counsel-projectile
    :bind
    ;; fuzzy find file in project
    ("s-f" . counsel-projectile-find-file)
    ;; fuzzy find phrase in project
    (:map projectile-command-map
          ("s" . counsel-projectile-rg)))
  ;; use perspective to manage project buffers
  (use-package persp-projectile
    :bind
    (:map projectile-mode-map
          ("s-s" . projectile-persp-switch-project)))
  ;; use git grep to ignore files
  (setq projectile-use-git-grep t)
  ;; use ivy as completion system
  (setq projectile-completion-system 'ivy))

;; diff-hl config
(use-package diff-hl
  :config
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode))

;; company config
(use-package company
  :diminish company-mode
  :bind
  (:map company-active-map
        ("M-n" . nil)
        ("M-p" . nil)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous))
  :init
  (add-hook 'after-init-hook 'global-company-mode))

;; aggressive-indent config
(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :init
  (add-hook 'after-init-hook 'aggressive-indent-global-mode))

;; easy-kill config
(global-set-key [remap kill-ring-save] 'easy-kill)

;; shows unbalanced delimiters
(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; autocomplete delimiters
(use-package smartparens
  :diminish smartparens-mode
  :init
  (add-hook 'after-init-hook 'smartparens-global-mode)
  :config
  ;; Org-mode config
  (sp-with-modes 'org-mode
    (sp-local-pair "'" nil :unless '(sp-point-after-word-p))
    (sp-local-pair "*" "*" :actions '(insert wrap) :unless '(sp-point-after-word-p sp-point-at-bol-p) :wrap "C-*" :skip-match 'sp--org-skip-asterisk)
    (sp-local-pair "_" "_" :unless '(sp-point-after-word-p))
    (sp-local-pair "/" "/" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "~" "~" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "=" "=" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "«" "»")))

;; flycheck config
(use-package flycheck
  :diminish flycheck-mode
  :init (global-flycheck-mode)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setq flycheck-highlighting-mode 'lines))

;; spell-checking in text modes with flyspell
(use-package flyspell
  :diminish flyspell-mode
  :init
  (setenv "DICTIONARY" "en_GB")
  :config
  (add-hook 'text-mode-hook 'flyspell-mode)
  ;; flyspell with aspell
  (setq ispell-list-command "--list"))

;; suggestive autocompletion for M-x
(use-package which-key
  :diminish which-key-mode
  :init (which-key-mode))

;; goto function definition
(use-package dumb-jump
  :bind
  (("M-g o" . dumb-jump-go-other-window)
   ("M-g j" . dumb-jump-go)
   ("M-g p" . dumb-jump-back)
   ("M-g i" . dumb-jump-go-prompt))
  :config
  (setq dumb-jump-selector 'ivy))

;; python stuff
(use-package anaconda-mode
  :diminish anaconda-mode
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  (use-package yapfify)
  :config
  (eval-after-load 'anaconda-mode
    '(progn
       (define-key anaconda-mode-map (kbd "M-s") 'yapfify-buffer))))

(use-package company-anaconda
  :config
  (eval-after-load "company"
    '(add-to-list 'company-backends '(company-anaconda))))

;; for Guru, we need to add https://github.com/dominikh/go-mode.el/blob/master/go-guru.el manually to a load path (which we also need to define)
(add-to-list 'load-path "~/.emacs.d/go/")
(require 'go-guru)

;; golang config
(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :config
  (add-hook 'go-mode-hook (lambda ()
                            (add-hook 'before-save-hook #'gofmt-before-save)
                            (setq gofmt-command "goimports")
                            (local-set-key (kbd "M-.") 'godef-jump)
                            (set (make-local-variable 'company-backends) '(company-go))
                            (company-mode)))
  (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)
  (use-package golint
    :config
    (add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
    (require 'golint))
  (use-package gorepl-mode
    :config (add-hook 'go-mode-hook #'gorepl-mode))
  (use-package company-go))
(setq company-go-show-annotation t)

;; use 4 spaces in protobuf-mode
(use-package protobuf-mode
  :init
  (defconst my-protobuf-style
    '((c-basic-offset . 4)
      (indent-tabs-mode . nil)))
  (add-hook 'protobuf-mode-hook
            (lambda () (c-add-style "my-style" my-protobuf-style t))))

;; vim-like expand-regions
(use-package expand-region
  :bind (("C-=" . er/expand-region)))

;; guru-mode
(use-package guru-mode
  :diminish guru-mode
  :init
  (add-hook 'after-init-hook 'guru-global-mode))

;; restclient
(use-package restclient
  :config
  (eval-after-load "restclient"
    '(add-to-list 'company-backends 'company-restclient)))

;; wgrep
(use-package wgrep
  :config (setq wgrep-enable-key "w"))

;; pdf
(use-package pdf-tools
  :mode (("\\.pdf\\'" . pdf-view-mode))
  ;; :bind
  ;; (:map pdf-view-mode-map
  ;;       (("h" . pdf-annot-add-highlight-markup-annotation)
  ;;        ("t" . pdf-annot-add-text-annotation)
  ;;        ("D" . pdf-annot-delete)
  ;;        ("C-s" . isearch-forward)))
  :config
  ;; More fine-grained resizing (10%)
  (setq pdf-view-resize-factor 1.1)

  ;; Install pdf tools
  (pdf-tools-install))

(use-package json-mode)

(use-package dockerfile-mode)

(use-package ibuffer-vc
  :init
  (add-hook 'ibuffer-hook
            (lambda()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(use-package git-link)

(use-package yasnippet
  :diminish yas-global-mode yas-minor-mode
  :init (add-hook 'after-init-hook 'yas-global-mode)
  :config (setq yas-snippet-dirs '("~/.emacs.d/snippets/snippets/")))
