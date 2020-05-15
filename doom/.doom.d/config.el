;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Lau Siaw Young"
      user-mail-address "sy@siawyoung.com"
      epa-file-encrypt-to user-mail-address)

;;; When starting up, maximize the window
(when IS-MAC
  (add-hook 'window-setup-hook #'toggle-frame-maximized))

(when (or IS-MAC IS-LINUX)
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-copy-env "GOROOT"))

(global-so-long-mode 1)

;; save recent files every 300 seconds
(run-at-time (current-time) 300 'recentf-save-list)

(add-hook 'prog-mode-hook 'turn-on-visual-line-mode)

(after! doom-modeline
  (setq doom-modeline-buffer-encoding nil)
  (setq size-indication-mode nil)
  (setq mode-line-percent-position nil)
  (setq line-number-mode nil)
  (setq column-number-mode nil))

(setq doom-themes-treemacs-enable-variable-pitch nil)

;; I don't want my lines to be automatically broken
(auto-fill-mode -1)

;; all popups to the side, please
(set-popup-rules!
  '(("*" :side right :width 0.25 :slot 1 :vslot 1 :quit nil)))

(if IS-MAC
(map!
 "s-f"  #'swiper-isearch
 "s-t"  #'+ivy/projectile-find-file
 (:leader
   :desc "Find file in project"  "SPC"  #'counsel-projectile-switch-project)
 (:when (featurep! :completion ivy)
        (:after ivy
          :map ivy-minibuffer-map
          ;; essentially, s-f s-f to resume your search
          "s-f"   #'previous-history-element
          ;; next-history-element will take the sexp under the cursor
          ;; if search field is empty
          "s-d"   #'next-history-element
          ;; open search results in a separate buffer
          "C-o"   #'ivy-occur)))

(map! "C-c C-r" #'rename-buffer)

(map!
 "M-f"  #'swiper-isearch
 "M-t"  #'+ivy/projectile-find-file
 (:leader
   :desc "Find file in project"  "SPC"  #'counsel-projectile-switch-project)
 (:when (featurep! :completion ivy)
        (:after ivy
          :map ivy-minibuffer-map
          ;; essentially, M-f M-f to resume your search
          "M-f"   #'previous-history-element
          ;; next-history-element will take the sexp under the cursor
          ;; if search field is empty
          "M-d"   #'next-history-element
          ; open search results in a separate buffer
          "C-o"   #'ivy-occur))))

(map! (:leader
        :desc "Open deadgrep" "d" #'deadgrep)
      (:leader
        :desc "Open a new vterm" "v" #'vterm)
      (:leader
        :desc "Open ranger" "r" #'ranger))

(use-package! deadgrep
  :config
  (defun deadgrep--format-command-patch (rg-command)
  "Add --hidden to rg-command."
  (replace-regexp-in-string "^rg " "rg --hidden " rg-command))
(advice-add 'deadgrep--format-command :filter-return #'deadgrep--format-command-patch))

(after! counsel
(setq counsel-find-file-ignore-regexp
        (concat
         ;; File names beginning with # or .
         "\\(?:\\`[#.]\\)"
         ;; File names ending with # or ~
         "\\|\\(?:\\`.+?[#~]\\'\\)")))

(after! company
  ;; disable automatic company suggestions, instead
  (setq company-idle-delay nil)
  ;; define custom keymapping for showing autocompletes
  (define-key evil-insert-state-map (kbd "C-c C-c") 'company-complete))

(after! flycheck
  ;; only flycheck on save, disable all other events
  (setq flycheck-check-syntax-automatically '(save)))


(use-package! lsp-python-ms
  :config
  (setq lsp-enable-snippet nil)
  :hook
  (python-mode . (lambda ()
                        (setq python-shell-interpreter "python")
                        (require 'lsp-python-ms)
                        (lsp))))

(use-package! pyenv-mode
  :diminish
  :config
  (pyenv-mode))

(use-package! python-black)

(map!
 "C-c C-x" #'python-shell-switch-to-shell
 "C-c C-a" #'run-python)

(with-eval-after-load 'python
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_"))))

(use-package! js2-mode
  :config
  (setq js2-highlight-level 3)
  (setq js2-mode-show-parse-errors t)
  (setq js2-mode-show-strict-warnings nil)
  (setq js2-strict-missing-semi-warning nil)
  (setq flycheck-javascript-eslint-executable "eslint_d"))

(use-package! prettier-js
  :hook
  ((js2-mode . prettier-js-mode)
   (typescript-mode . prettier-js-mode)
   (rjsx-mode . prettier-js-mode)))

(use-package! add-node-modules-path
  :hook ((js2-mode . add-node-modules-path)
         (typescript-mode . add-node-modules-path)
         (rjsx-mode . add-node-modules-path)))

;; associate .tsx files with typescript-mode
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

(after! forge
  ;; I have way too many unread github notifications, running this will hang Emacs for days
  (setq forge-pull-notifications nil))

(after! persistent-scratch
  (persistent-scratch-setup-default)
  (setq persistent-scratch-autosave-mode 1))
(add-hook 'go-mode-hook (lambda()
                          (add-hook 'before-save-hook #'gofmt-before-save)
                          (setq gofmt-command "goimports")))

(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
(after! emmet
  (setq emmet-expand-jsx-className? t))

(map!
 "C-c C-b" #'emmet-expand-line)

(map!
 "M-g j" #'dumb-jump-go
 "M-g b" #'dumb-jump-back
 "M-g o" #'dumb-jump-go-other-window)

(after! vue-mode
  (setq mmm-submode-decoration-level 0))

(after! git-auto-commit-mode
  (setq gac-automatically-add-new-files-p nil)
  (setq gac-automatically-push-p t))

(after! lsp-ui
  (add-hook 'scala-mode-hook (lambda()
                               (lsp-ui-mode)
                               (lsp-ui-doc-mode))))

(load! "+theming")
(load! "+navigation")
(load! "+vc")
(load! "+functions")
(load! "+org")
