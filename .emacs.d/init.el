;;; init.el --- This is my Emacs configuration

;; whoami
(setq user-full-name "Lau Siaw Young"
      user-mail-address "lausiawyoung@gmail.com")

;; load customize config
(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file) (load custom-file))

;; load secrets
;; (load-file "~/.emacs.d/secrets.el")

;; add package archives
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

;; use-package
(unless (and (package-installed-p 'use-package)
             (package-installed-p 'delight)
             (package-installed-p 'diminish))
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'delight)
  (package-install 'diminish))

(eval-when-compile
  (defvar use-package-verbose t)
  (require 'use-package)
  (require 'diminish)
  (require 'delight)
  (require 'bind-key))

;; always ensure
(setq use-package-always-ensure t)
;; Always demand if daemon-mode
(setq use-package-always-demand (daemonp))

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

;; save all buffers when Emacs loses focus
(add-hook 'focus-out-hook
          (lambda () (save-some-buffers t)))

;; load $PATH env variable into emacs
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

;; set universal font size to 17px
(set-face-attribute 'default nil :height 170)

(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.emacs-saves"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs-saves/" t)))

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
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture))
  :config
  (setq
   org-modules (quote (org-habit))
   ;; Add syntax highlighting to code blocks
   org-src-fontify-natively t
   org-src-tab-acts-natively t))

;; set timestamp when marked as done
(setq org-log-done 'time)

(setq org-return-follows-link t)
;; soft-wrap lines
(setq org-startup-truncated nil)
(setq org-agenda-files '("~/github/org/gtd/inbox.org"
                         "~/github/org/gtd/gtd.org"
                         "~/github/org/gtd/tickler.org"))

(set-register ?i (cons 'file "~/github/org/gtd/inbox.org"))
(set-register ?g (cons 'file "~/github/org/gtd/gtd.org"))
(set-register ?x (cons 'file "~/github/dotfiles/.emacs.d/init.el"))

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/github/org/gtd/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("r" "Tickler" entry
                               (file+headline "~/github/org/gtd/tickler.org" "Tickler")
                               "* %i%? \n %U")))

;; set timestamp when refiling
(setq org-log-refile 'time)
(setq org-refile-targets '(("~/github/org/gtd/gtd.org" :maxlevel . 3)
                           ("~/github/org/gtd/someday.org" :level . 1)
                           ("~/github/org/gtd/tickler.org" :maxlevel . 2)))

(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)

;;;
;; Third party package config
;;;

;; load gruvbox
(load-theme 'gruvbox t)

;; make selected text background easier to see
(set-face-attribute 'region nil :background "#666")

(use-package centered-cursor-mode
  :diminish centered-cursor-mode
  :init
  (add-hook 'after-init-hook 'global-centered-cursor-mode))

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
  (setq counsel-rg-base-command "rg -S --no-heading --line-number --color never %s . | cut -c -200")
  (setq counsel-find-file-occur-cmd "ls -a | grep -i -E '%s' | tr '\\n' '\\0' | xargs -0 ls -d")
  ;; 2 buffers with the same name will have the path prepended
  (setq ivy-virtual-abbreviate "full")
  (setq ivy-count-format "(%d/%d) ")
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
  :bind (("s-g" . 'save-and-magit-status))
  :config
  (defun save-and-magit-status ()
    "Save all buffers before opening magit status."
    (interactive)
    (save-some-buffers t)
    (magit-status))
  ;; find all git projects using magit
  (setq magit-repository-directories
        '(("~/github/" . 1)
          ("~/github/go/src/github.com/carousell/" . 1))))

;; reload buffers if changes happen in buffers due to git
(diminish 'auto-revert-mode)
(global-auto-revert-mode 1)

;; projectile config
(use-package projectile
  ;; show only the project name in mode line
  :delight '(:eval (concat " " (projectile-project-name)))
  :init
  (add-hook 'after-init-hook 'projectile-mode)
  :config
  (setq projectile-enable-caching t)
  ;; https://emacs.stackexchange.com/questions/32634/how-can-the-list-of-projects-used-by-projectile-be-manually-updated/3
  (when (require 'magit nil t)
    (mapc #'projectile-add-known-project
          (mapcar #'file-name-as-directory (magit-list-repos)))
    ;; Optionally persist
    (projectile-save-known-projects))
  (use-package counsel-projectile
    :bind
    ;; open project search
    ("s-s" . projectile-switch-project)
    ;; fuzzy find file in project
    ("s-f" . counsel-projectile-find-file)
    ;; fuzzy find phrase in project
    (:map projectile-command-map
          ("s" . counsel-projectile-rg)))
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
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (use-package company-childframe
    :diminish
    :config
    (company-childframe-mode 1)))

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

(use-package lsp-mode
  :config
  (require 'lsp-imenu)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
  (lsp-define-stdio-client lsp-python "python"
                           #'projectile-project-root
                           '("pyls"))
  (add-hook 'python-mode-hook
            (lambda ()
              (lsp-python-enable)))
  (use-package lsp-go
    :config
    (add-hook 'go-mode-hook #'lsp-go-enable))
  (use-package lsp-ui
    :config
    (setq lsp-ui-sideline-ignore-duplicate t)
    (add-hook 'lsp-mode-hook 'lsp-ui-mode))
  (use-package company-lsp
    :config
    (push 'company-lsp company-backends)))

;; python stuff
(use-package pyimpsort)
(use-package pyenv-mode)

;; for Guru, we need to add https://github.com/dominikh/go-mode.el/blob/master/go-guru.el manually to a load path (which we also need to define)
(add-to-list 'load-path "~/.emacs.d/go/")
(require 'go-guru)

;; golang config
(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :config
  (add-hook 'go-mode-hook (lambda ()
                            ;; http://ergoemacs.org/emacs/emacs_subword-mode_superword-mode.html
                            (subword-mode 1)
                            ;; disable aggressive-indent-mode as a temporary solution to make
                            ;; company suggestions work properly
                            (aggressive-indent-mode 0)
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
    :diminish
    :config (add-hook 'go-mode-hook #'gorepl-mode))
  (use-package company-go)
  (use-package go-playground))
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

(use-package git-link
  :config
  (setq git-link-use-commit t))

(use-package yasnippet
  :diminish yas-global-mode yas-minor-mode
  :config
  (use-package yasnippet-snippets)
  (add-to-list 'yas-snippet-dirs yasnippet-snippets-dir)
  (yas-global-mode 1)
  (global-set-key (kbd "M-/") 'company-yasnippet))

(use-package golden-ratio
  :diminish golden-ratio-mode
  :init
  (add-hook 'after-init-hook 'golden-ratio-mode)
  :config
  (add-to-list 'golden-ratio-extra-commands 'ace-window))

(use-package crux
  :bind (("C-c C" . crux-cleanup-buffer-or-region)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-a" . crux-move-beginning-of-line)
         ("M-o" . crux-smart-open-line)
         ("C-c r" . crux-rename-file-and-buffer)
         ("M-D" . crux-duplicate-and-comment-current-line-or-region)
         ("s-o" . crux-smart-open-line-above)))

(use-package key-chord
  :config
  ;; for double-press on same key, increase the delay a bit more
  (setq key-chord-one-key-delay 0.3)
  (key-chord-mode 1)
  (key-chord-define-global "jj" 'ace-swap-window)
  (key-chord-define-global "kk" 'ace-window)
  ;; vim-like
  (key-chord-define-global "vv" 'er/expand-region))


(use-package ace-window
  :config
  (setq aw-swap-invert t))

(use-package smartscan
  :init
  (add-hook 'after-init-hook 'global-smartscan-mode))

(use-package multiple-cursors
  :bind (("C-M-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package smex)

(use-package avy
  :config
  (setq avy-timeout-seconds 0.2)
  :bind (("s-w" . avy-goto-char-timer)
         ("M-g g" . avy-goto-line)))

(use-package ivy-posframe
  :diminish
  :config
  (setq ivy-display-functions-alist
        '((counsel-M-x . ivy-posframe-display-at-point)
          (projectile-switch-project . ivy-posframe-display-at-point)
          (counsel-projectile-find-file . ivy-posframe-display-at-point)
          (ivy-switch-buffer . ivy-posframe-display-at-point)
          (counsel-find-file . ivy-posframe-display-at-point)
          (ivy-completion-in-region . ivy-display-function-overlay)
          ;; fallback
          (t . ivy-posframe-display))))
