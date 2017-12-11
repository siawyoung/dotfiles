;;; init.el --- This is my Emacs configuration

;; whoami
(setq user-full-name "Lau Siaw Young"
      user-mail-address "lausiawyoung@gmail.com")
;; end

;; load customize config
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
;; end

;; add package archives
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)
;; end

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
;; end

;; run recentf every 5 minutes
(require 'recentf)
(run-at-time (* 5 60) nil
             (lambda ()
               (let ((inhibit-message t))
                 (recentf-save-list))))
;; end

;; validate config
;; check for emacs config errors
(use-package validate
  :demand t)
;; end

;; prefer y/n
(defalias 'yes-or-no-p 'y-or-n-p)
;; end

;; load $PATH env variable into emacs
(use-package exec-path-from-shell
  :demand t
  :init (exec-path-from-shell-initialize))
;; end

;; set universal font size to 17px
(set-face-attribute 'default nil :height 170)
;; end

;; Move all backup files to the OS's temp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;; end

;; Remove useless toolbars and splash screens
(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
;; end

;; Use single spaces
(setq sentence-end-double-space nil)
;; end

;; Use 2 spaces for tabs
(setq-default tab-width 2)
(setq-default js-indent-level 2)
(setq-default indent-tabs-mode nil)
;; end

;; show whitespace
;; (require 'whitespace)
;; (diminish 'whitespace-mode)
;; (add-hook 'prog-mode-hook 'whitespace-mode)
;; end

;; delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; end

;; stop blinking cursor
(blink-cursor-mode 0)
;; end

;;;
;; Third party package config
;;;

;; load gruvbox
(load-theme 'gruvbox t)
;; end

;; counsel config
(use-package counsel
  :demand t
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
;; end

;; flx
(use-package flx)

;; magit config
(use-package magit
  :bind (("s-g" . magit-status)))
;; end

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
  (use-package counsel-projectile
    :bind
    ;; fuzzy find file in project
    ("s-f" . counsel-projectile-find-file)
    ;; fuzzy find phrase in project
    (:map projectile-command-map
          ("s" . counsel-projectile-rg))
    :config (counsel-projectile-on))
  ;; use git grep to ignore files
  (setq projectile-use-git-grep t)
  ;; use ivy as completion system
  (setq projectile-completion-system 'ivy))
;; end

;; diff-hl config
(use-package diff-hl
  :config
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode))
;; end

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
;; end

;; aggressive-indent config
(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :init
  (add-hook 'after-init-hook 'aggressive-indent-global-mode))
;; end

;; easy-kill config
(global-set-key [remap kill-ring-save] 'easy-kill)
;; end

;; shows unbalanced delimiters
(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; autocomplete delimiters
(use-package smartparens
  :diminish smartparens-mode
  :init
  (add-hook 'after-init-hook 'smartparens-global-mode))

;; flycheck config
(use-package flycheck
  :init (global-flycheck-mode)
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; suggestive autocompletion for M-x
(use-package which-key
  :diminish which-key-mode
  :init (which-key-mode))

;; goto function definition
(use-package dumb-jump
  :bind
  (("M-g o" . dumb-jump-go-other-window)
   ("M-g j" . dumb-jump-go)
   ("M-g i" . dumb-jump-go-prompt))
  :config
  (setq dumb-jump-selector 'ivy))

;; python stuff
(use-package anaconda-mode
  :diminish anaconda-mode
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package company-anaconda
  :config
  (eval-after-load "company"
    '(add-to-list 'company-backends '(company-anaconda))))
