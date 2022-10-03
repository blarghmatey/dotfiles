;;; package --- Summary
;; Commentary:
;;; This file pulls in the functions and variables from functions.el and
;;; configurations.el as well as providing initialization routines

;;; Code:

;;; Package manager settings
(setq tls-checktrust t)
(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string "python -m certifi")))))
  (setq tls-program
        (list
         (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                 (if (eq window-system 'w32) ".exe" "") trustfile)))
  (setq gnutls-verify-error t)
  (setq gnutls-trustfiles (list trustfile)))

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(require 'cask)
(cask-initialize)

(setq load-path (append load-path '("~/.emacs.d/plugins")))

(global-whitespace-mode)
(global-auto-revert-mode t)
(display-time-mode 1)
;; http://news.ycombinator.com/item?id=873541
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'program-mode-hook 'default-minor-modes)
;; (add-hook 'python-mode-hook 'flycheck-python-setup)
(add-hook 'yaml-mode-hook (lambda () (setq-local electric-indent-mode -1)))
(add-hook 'markdown-mode-hook (lambda () (setq-local whitespace-style
                                                     '(face trailing empty tab-mark))))
(add-hook 'org-mode-hook (lambda () (setq-local whitespace-style
                                                '(face trailing empty tab-mark))))
(add-hook 'org-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name (current-buffer))))
              (when (and filename (string= "trello" (file-name-extension filename)))
                (org-trello-mode)))))
(add-hook 'org-mime-html-hook
          (lambda ()
            (org-mime-change-element-style
             "blockquote" "border-left: 2px solid gray; padding-left: 4px;")))

(winner-mode)

(use-package ace-window
  :ensure t
  :delight
  :bind ("C-x o" . ace-window)
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
                aw-scope 'frame))

(use-package ag
  :ensure t
  :defer t)

(use-package company
  :ensure t
  :delight
  :hook (after-init . global-company-mode)
  :bind ("C-." . company-complete)
  :config (setq company-idle-delay 2
                company-tooltip-idle-delay 0.5
                company-auto-commit nil
                company-minimum-prefix-length 1
                company-echo-truncate-lines nil
                company-frontends '(company-pseudo-tooltip-unless-just-one-frontend-with-delay
                                    company-echo-metadata-frontend
                                    company-preview-if-just-one-frontend)
                company-search-regexp-function 'company-search-flex-regexp
                company-tooltip-maximum-width 80))

(use-package company-quickhelp
  :ensure t)

(use-package csv-mode
  :ensure t)

(use-package delight
  :ensure t)

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package dockerfile-mode
  :ensure t)

(use-package dumb-jump
  :ensure t)

(use-package editorconfig
  :ensure t
  :delight
  :hook (prog-mode . editorconfig-mode))

(use-package editorconfig-generate
  :ensure t)

(use-package ef-themes
  :ensure t)

;; Emacs IPython/Jupyter Notebooks
(use-package ein
  :ensure t)

(use-package elpy
  :ensure t
  :delight
  (elpy-mode)
  (hl-line-mode)
  (subword-mode)
  (highlight-indentation-mode)
  (hs-minor-mode)
  (eldoc-mode)
  :after python
  ;; :hook (elpy-mode . (subword-mode hl-line-mode flycheck-mode))
  :config
  (setq elpy-modules '(elpy-module-company
                       elpy-module-django
                       elpy-module-autodoc
                       elpy-module-eldoc
                       elpy-module-folding
                       elpy-module-pyvenv
                       elpy-module-highlight-indentation
                       elpy-module-yasnippet)
        elpy-test-runner 'elpy-test-pytest-runner
        elpy-formatter "Black")
  (elpy-enable))

(use-package fill-column-indicator
  :ensure t
  :delight fci-mode
  :hook (python-mode . fci-mode)
  :config (setq fill-column 88
                fci-rule-column 88
                fci-rule-color "#37474f"))

(use-package flycheck
  :ensure t
  :delight flycheck-mode
  :hook (after-init . global-flycheck-mode)
  :init
  (use-package flycheck-mypy :ensure t)
  :config (setq flycheck-disabled-checkers '(python-pycompile python-pylint)
                flycheck-flake8rc "setup.cfg"
                flycheck-python-pyright-executable "~/.emacs.d/.cache/lsp/npm/pyright/bin/pyright"
                flycheck-python-mypy-config "setup.cfg"))

(use-package flycheck-projectile
  :ensure t)

(use-package forge
  :ensure t
  :after magit
  :config (setq forge-topic-list-limit 0))

(use-package frontside-javascript
  :ensure t)

(use-package git-gutter-fringe+
  :ensure t
  :demand t
  :hook (after-init . global-git-gutter+-mode)
  :delight git-gutter+-mode)

(use-package git-link
  :ensure t)

(use-package github-review
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package hcl-mode
  :ensure t)

(use-package helm
 :ensure t
 :delight helm-mode
 :init
 (helm-mode)
 ;; (use-package helm-flx :ensure t)
 ;; (helm-flx-mode)
 :config
 (setq helm-mode-fuzzy-match t
       helm-completion-in-region-fuzzy-match t
       helm-M-x-fuzzy-match t
       helm-buffers-fuzzy-matching t
       helm-completion-style 'emacs
       completion-styles `(basic partial-completion emacs22 initials))
 (helm-autoresize-mode 1)
 :bind (("M-x" . helm-M-x)
        ("C-x C-f" . helm-find-files)
        ("C-x b" . helm-buffers-list)))

(use-package helm-ag
 :ensure t
 :after (:all helm ag))

(use-package helm-company
 :ensure t)

(use-package helm-lsp
 :ensure t
 :commands helm-lsp-workspace-symbol)

(use-package helm-projectile
 :ensure t
 :after (:all projectile helm)
 :config
 (helm-projectile-on)
 (setq projectile-mode-line-prefix ""))

(use-package indent-control
  :ensure t)

(use-package jinja2-mode
  :ensure t
  :mode (("\\.j2" . jinja2-mode)
         ("\\.jinja" . jinja2-mode)))

(use-package json-mode
  :ensure t)

(use-package k8s-mode
 :ensure t
 :config
 (setq k8s-search-documentation-browser-function 'browse-url-firefox
       k8s-site-docs-version "v1.22"
       k8s-site-docs-url "https://kubernetes.io/docs/reference/generated/kubernetes-api/")
 :hook (k8s-mode . yas-minor-mode))

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview)
  :bind ("C-x K" . kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))

(use-package lice
  :ensure t)

(use-package linum-relative
  :ensure t
  :bind ("C-c t l" . linum-relative-toggle))

(use-package lsp-mode
  :ensure t
  :delight
  :hook
  (js-mode . lsp-deferred)
  (json-mode . lsp-deferred)
  (shell-mode . lsp-deferred)
  (html-mode . lsp-deferred)
  (web-mode . lsp-deferred)
  (python-mode . lsp-deferred)
  (dockerfile-mode . lsp-deferred)
  (go-mode . lsp-deferred)
  (php-mode . lsp-deferred)
  :bind
  (:map lsp-mode-map)
  ("M-." . xref-find-definitions)
  :bind-keymap ("C-;" . lsp-command-map)
  :init
  (setq lsp-keymap-prefix "C-;"
        gc-cons-threshold 3200000
        lsp-auto-configure t
        lsp-before-save-edits nil
        lsp-diagnostic-package :none
        lsp-enable-completion-at-point t
        lsp-enable-folding t
        lsp-enable-file-watchers t
        lsp-enable-imenu t
        lsp-enable-imenu t
        lsp-enable-indentation t
        lsp-enable-semantic-highlighting t
        lsp-enable-text-document-color t
        lsp-file-watch-threshold 15000
        lsp-imenu-show-container-name t
        lsp-log-io nil
        lsp-modeline-code-actions-mode t
        lsp-prefer-capf t
        lsp-keep-workspace-alive nil
        lsp-semantic-highlighting :immediate
        read-process-output-max (* 1024 1024))
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.?venv\\'"))
  :config (advice-add 'lsp :before
                      (lambda (&rest _args)
                        (eval
                         '(setf
                           (lsp-session-server-id->folders (lsp-session))
                           (ht)))))
  :commands (lsp lsp-deferred))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp)))
  :config (setq lsp-pyright-diagnostic-mode "workspace"
                lsp-pyright-use-library-code-for-types t
                lsp-pyright-auto-import-completions t
                lsp-pyright-disable-organize-imports t
                lsp-pyright-typechecking-mode "off"))

(use-package lsp-java
  :ensure t
  :config
  (add-hook 'java-mode-hook 'lsp)
  (setq lsp-java-completion-enabled t))

(use-package lsp-ui
  :ensure t
  :bind (
         :map lsp-command-map
              ("G i" . lsp-ui-imenu)
              ("G d" . lsp-ui-doc-show)
              ("G h" . lsp-ui-doc-hide)
              ("G f" . lsp-ui-doc-focus-frame)
              ("G u" . lsp-ui-doc-unfocus-frame))
  :commands lsp-ui-mode
  :config (setq lsp-ui-doc-enable t
                lsp-ui-doc-position 'at-point
                lsp-ui-doc-alignment 'frame
                lsp-ui-doc-max-height 40
                lsp-ui-doc-show-with-cursor nil
                lsp-ui-sideline-update-mode "line"
                lsp-ui-sideline-show-hover t
                lsp-ui-sideline-show-code-actions t
                lsp-ui-sideline-show-diagnostics t
                lsp-ui-sideline-diagnostic-max-line-length 120
                lsp-ui-doc-include-signature t
                lsp-ui-doc-header t))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config (setq magit-last-seen-setup-instructions "1.4.0"
                magit-commit-arguments '("-S")
                magit-log-section-arguments '("-n256" "--decorate")))

(use-package magit-delta
  :ensure t)

(use-package markdown-changelog
  :ensure t)

(use-package mmm-mode
  :ensure t
  :delight)

;; (use-package modus-themes
;;   :ensure
;;   :init
;;   ;; Add all your customizations prior to loading the themes
;;   (setq modus-themes-bold-constructs t
;;         modus-themes-paren-match '(underline)
;;         modus-themes-markup '(intense)
;;         modus-themes-fringes '(subtle)
;;         modus-themes-hl-line '(accented)
;;         modus-themes-syntax '(alt-syntax green-strings yellow-comments)
;;         modus-themes-region '(bg-only no-extend))

;;   ;; Load the theme files before enabling a theme
;;   (modus-themes-load-themes)
;;   :config
;;   ;; Load the theme of your choice:
;;   (modus-themes-load-vivendi);; OR (modus-themes-load-operandi)
;;   :bind ("<f5>" . modus-themes-toggle))

(use-package nginx-mode
  :ensure t)

(use-package obsidian
  :ensure t
  :demand t
  :config
  (obsidian-specify-path "~/Dropbox/Obsidian-Notes/")
  (global-obsidian-mode t)
  :custom
  ;; This directory will be used for `obsidian-capture' if set.
  (obsidian-inbox-directory "Inbox")
  :bind (:map obsidian-mode-map
              ;; Replace C-c C-o with Obsidian.el's implementation. It's ok to use another key binding.
              ("C-c C-o" . obsidian-follow-link-at-point)
              ;; If you prefer you can use `obsidian-insert-wikilink'
              ("C-c C-l" . obsidian-insert-wikilink)))

(use-package org
  :ensure t
  :bind (("C-c L" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c s e" . org-edit-src-code)
         ("C-c o n" lambda () (interactive) (find-file (concat org-directory "notes/")))
         ("C-c o t" lambda () (interactive) (find-file (concat org-directory "todo/")))
         ("C-c o T" lambda () (interactive) (find-file (concat org-directory "trello/"))))
  :config
  (setq org-directory "~/Dropbox/org/"
        org-log-done t
        org-log-redeadline (quote time)
        org-log-reschedule (quote time)
        org-log-into-drawer t
        org-agenda-restore-windows-after-quit t
        org-use-sub-superscripts '{}
        org-export-with-sub-superscripts '{}
        org-todo-keywords
        '((sequence "TODO(t)" "DOING(d!)" "|" "DONE(D!)" "CANCELLED(C!)"))
        org-todo-keyword-faces
        '(("TODO" . org-warning) ("DOING" . "yellow") ("DONE" . (:foreground "green" :weight bold)))
        org-agenda-files
        (quote
         ("~/Dropbox/org/todo/" "~/Dropbox/org/calendars/" "~/Dropbox/org/journal"))
        org-default-notes-file (concat org-directory "notes.org")
        org-refile-targets `((org-agenda-files . (:maxlevel . 2)))
        org-capture-templates
        `(("t" "Todo" entry (file ,(concat org-directory "todo/todo.org"))
           "** TODO %? :%^G\n:PROPERTIES:\n:Created: %U\n:END:")
          ("n" "Note" entry (file org-default-notes-file)
           "* %?\n:PROPERTIES:\n:Created: %U\n:END:")
          ("f" "Followup" entry (file+headline ,(concat org-directory "todo/todo.org") "Tasks")
           "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n"))))

(use-package org-journal
  :ensure t
  :config (setq org-journal-date-format "%Y-%m-%d"
                org-journal-dir "~/Dropbox/org/journal/"
                org-journal-file-format "%Y-%m-%d.org"
                org-journal-search-results-order-by :desc))

(use-package org-trello
  :ensure t
  :mode ("\\.trello" . org-mode))

(use-package ox-hugo
  :ensure t)

(use-package perspective
  :ensure t
  :bind-keymap ("C-z" . perspective-map)
  :custom (persp-mode-prefix-key (kbd "C-z"))
  :bind (:map perspective-map
              ("c" . persp-switch)
              ("k" . persp-kill)
              ("d" . persp-remove-buffer))
  :init (setq persp-state-default-file "~/.emacs.d/perspective_state.el")
  :config
  (persp-mode)
  (run-with-timer 300 (* 5 60) 'persp-state-save "~/.emacs.d/perspective_state.el"))

(use-package php-mode
  :ensure t)

(use-package poetry
  :ensure t
  :config
  (poetry-tracking-mode))

(use-package projectile
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (("M-P" . projectile-find-file)
         :map projectile-command-map
         ("p" . projectile-switch-project)
         ("C-c" . flycheck-projectile-list-errors))
  :init (setq projectile-completion-system 'helm)
  :hook (after-init . projectile-mode))

(use-package py-isort
  :ensure t
  :config (setq py-isort-options '("-w=120" "--balanced" "-m=3")))

(use-package python-insert-docstring
  :ensure t)

(use-package python-docstring
  :ensure t
  :delight
  :hook (python-mode . python-docstring-mode))

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1))

(use-package salt-mode
  :ensure t)

(use-package sphinx-doc
  :ensure t
  :delight sphinx-doc-mode
  :hook (python-mode . sphinx-doc-mode))

(use-package sphinx-mode
  :ensure t
  :delight sphinx-mode
  :hook (python-mode . sphinx-mode))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-perspective ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs perspective) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package typescript-mode
  :ensure t)

(use-package undo-tree
  :ensure t
  :delight undo-tree-mode
  :hook (after-init . global-undo-tree-mode)
  :config (setq undo-tree-auto-save-history t
                undo-tree-enable-undo-in-region t
                undo-tree-visualizer-diff t
                undo-tree-visualizer-timestamps t
                undo-tree-history-directory-alist '(("." . "/home/tmacey/.emacs.d/undo-tree/"))))

(use-package lush-theme
  :ensure t)

(use-package web-mode
  :ensure t
  :mode ("\\.html" . web-mode))

(use-package wgrep-ag
  :ensure t)

(use-package which-key
  :ensure t
  :demand t
  :delight which-key-mode
  :init
  (which-key-setup-side-window-bottom)
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'bottom)
  :config (which-key-mode)
  :commands which-key-mode)

(use-package xonsh-mode
  :ensure t)

(use-package yasnippet
  :ensure t
  :delight yas
  :init (yas-reload-all)
  :hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets :ensure t)

(load-file "~/.emacs.d/functions.el")
(load-file "~/.emacs.d/configurations.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#ffffff" "#f36c60" "#8bc34a" "#fff59d" "#4dd0e1" "#b39ddb" "#81d4fa" "#263238"))
 '(custom-safe-themes
   '("9b7d703afa15e320af9aa6b3e02bb1ebb8657c23043f7054357f86ce014b5461" "0f964c8dbc5a668cc2ba7aa1003792fbbf3000a6ed69c0e53b1eeb2c1afc25cb" "b87f0a7cc94fc07f1417f95de2382a7c1c853a6822d987af45b3b3c5e95e3abb" "9f97708991e9b0ddc2d428e5bae87d97d8b6c6c09ef82cbfa26a797560de7cec" "6abef8c5e70ae252c41e9c91a885635de66816204a0bd9102387f6f7c419a7a5" "2480d2400cf9eb2f58703d0f3e6ae23b15d8bc6c7b8070c65328f32f31df6a03" "a2c06295bcca9ffc56f22b4d1f1f145cf9a6953785099199afe2e4db75e54630" "448175cf1da8bdb6dd310a33283789bdb5b76402adcc0a23bcebeb3e6f667bdf" "13f4cc4607ec8c2aada98ee92293547d7134ffa4d746c9104647461bcbcab8a7" "cda446bf884e720497d5f1463ff699896c5de42352321c7bc91e637da25cad2e" "dd64cf49a64a5adeebc30a8e968db73f549e11d6adcd1318893fded4ee0b214b" "0c2d7f410f835d59a0293f2a55744e9d3be13aab8753705c6ad4a9a968fb3b28" "364e592858d85f3dff9d51af5c72737ca8ef2b76fc60d9d5f0a9995ea927635a" "0f2f1feff73a80556c8c228396d76c1a0342eb4eefd00f881b91e26a14c5b62a" default))
 '(debug-on-error nil)
 '(helm-completion-style 'emacs)
 '(horizontal-scroll-bar-mode nil)
 '(org-trello-current-prefix-keybinding "C-c o" nil (org-trello))
 '(package-selected-packages
   '(json-mode sphinx-mode sphinx-doc python-insert-docstring ef-themes obsidian modus-themes lsp-java treemacs-perspective treemacs-magit treemacs-icons-dired treemacs-projectile treemacs vcl-mode k8s-mode kubernetes ox-hugo frontside-javascript indent-control ein undo-tree go-mode typescript-mode hcl-mode ace-window flycheck-projectile lsp-pyright hybrid-reverse-theme company-quickhelp helm-company editorconfig-generate editorconfig dhall-mode yasnippet-snippets xonsh-mode which-key wgrep-ag web-mode use-package salt-mode python-docstring py-isort poetry php-mode perspective pallet org-trello org-journal nginx-mode markdown-changelog magit-delta lush-theme lsp-ui linum-relative lice jinja2-mode helm-projectile helm-lsp helm-flx helm-ag github-review git-link git-gutter-fringe+ forge flycheck-mypy fill-column-indicator elpy dockerfile-mode docker delight ag))
 '(safe-local-variable-values
   '((flycheck-checker . "python-flakehell")
     (flycheck-checker quote python-flakehell)
     (flycheck-disable-checkers quote
                                (python-flake8 python-pylint python-pycompile))
     (flycheck-disable-checker quote
                               (python-flake8 python-pylint python-pycompile))
     (lambda nil
       (flycheck-select-checker 'python-flakehell))
     (flycheck-select-checker . python-flakehell)
     (flycheck-disabled-checkers quote
                                 (python-pycompile python-pylint python-flake8))))
 '(scroll-bar-mode nil)
 '(url-handler-mode t))

(set-cursor-color "white")

(provide 'init)
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
