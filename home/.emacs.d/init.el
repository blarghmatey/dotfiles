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
(require 'cask "~/.cask/cask.el")
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
                company-auto-complete nil
                company-minimum-prefix-length 1
                company-echo-truncate-lines nil
                company-frontends '(company-pseudo-tooltip-unless-just-one-frontend-with-delay
                                    company-echo-metadata-frontend
                                    company-preview-if-just-one-frontend)
                company-search-regexp-function 'company-search-flex-regexp
                company-tooltip-maximum-width 80))

(use-package company-quickhelp
  :ensure t)

(use-package delight
  :ensure t)

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package dockerfile-mode
  :ensure t)

(use-package editorconfig
  :ensure t
  :delight
  :hook (prog-mode . editorconfig-mode))

(use-package editorconfig-generate
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
                       elpy-module-yasnippet
                       elpy-module-sane-defaults)
        elpy-rpc-backend "jedi"
        elpy-test-runner 'elpy-test-pytest-runner)
  (elpy-enable))

(use-package fill-column-indicator
  :ensure t
  :delight fci-mode
  :hook (python-mode . fci-mode)
  :config (setq fci-rule-column 120
                fci-rule-color "#37474f"))

(use-package flycheck
  :ensure t
  :delight flycheck-mode
  :hook (after-init . global-flycheck-mode)
  :init
  (use-package flycheck-mypy :ensure t)
  :config (setq flycheck-disabled-checkers '(python-pycompile python-pylint)
                flycheck-flake8-maximum-complexity 15
                flycheck-flake8rc "setup.cfg"
                flycheck-python-mypy-config "setup.cfg"))

(use-package flycheck-projectile
  :ensure t)

(use-package forge
  :ensure t
  :after magit
  :config (setq forge-topic-list-limit 0))

(use-package git-gutter-fringe+
  :ensure t
  :demand t
  :hook (after-init . global-git-gutter+-mode)
  :delight git-gutter+-mode)

(use-package git-link
  :ensure t)

(use-package github-review
  :ensure t)

(use-package hcl-mode
  :ensure t
  :mode ("\.nomad" . hcl-mode))

(use-package helm
  :ensure t
  :delight helm-mode
  :init
  (helm-mode)
  (use-package helm-flx :ensure t)
  (helm-flx-mode)
  :config
  (setq helm-mode-fuzzy-match t
        helm-completion-in-region-fuzzy-match t
        helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-completion-style 'emacs
        completion-styles `(basic partial-completion emacs22 initials
                                  ,(if (version<= emacs-version "27.0") 'helm-flex 'flex)))
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

(use-package jinja2-mode
  :ensure t
  :mode (("\\.j2" . jinja2-mode)
         ("\\.jinja" . jinja2-mode)))

(use-package js2-mode
  :ensure t
  :delight js2-minor-mode
  :hook (js-mode . js2-minor-mode))

(use-package lice
  :ensure t)

(use-package linum-relative
  :ensure t
  :bind ("C-c t l" . linum-relative-toggle))

(use-package lsp-mode
  :ensure t
  :delight
  :hook
  (js-mode . lsp)
  (json-mode . lsp)
  (shell-mode . lsp)
  (html-mode . lsp)
  (web-mode . lsp)
  (python-mode . lsp)
  (dockerfile-mode . lsp)
  :bind ("M-." . xref-find-definitions)
  :init (setq-default gc-cons-threshold 3200000
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
                      lsp-file-watch-threshold 100000
                      lsp-imenu-show-container-name t
                      lsp-keymap-prefix "C-;"
                      lsp-modeline-code-actions-mode t
                      lsp-prefer-capf t
                      lsp-semantic-highlighting :immediate
                      read-process-output-max (* 1024 1024))
  :commands (lsp))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp)))
  :config (setq lsp-pyright-diagnostic-mode "workspace"
                lsp-pyright-use-library-code-for-types t
                lsp-pyright-auto-import-completions t
                lsp-pyright-disable-organize-imports t))

(use-package lsp-ui
  :ensure t
  :bind
  ("C-; G i" . lsp-ui-imenu)
  ("C-; G d" . lsp-ui-doc-show)
  ("C-; G h" . lsp-ui-doc-hide)
  ("C-; G f" . lsp-ui-doc-focus-frame)
  ("C-; G u" . lsp-ui-doc-unfocus-frame)
  :commands lsp-ui-mode
  :config (setq lsp-ui-doc-position 'top
                lsp-ui-doc-alignment 'frame
                lsp-ui-doc-max-height 40
                lsp-ui-sideline-mode "line"
                lsp-ui-sideline-show-hover t
                lsp-ui-sideline-show-code-actions t
                lsp-ui-sideline-show-diagnostics t
                lsp-ui-doc-include-signature nil
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

(use-package nginx-mode
  :ensure t)

(use-package org-journal
  :ensure t
  :config (setq org-journal-date-format "%Y-%m-%d"
                org-journal-dir "~/Dropbox/org/journal/"
                org-journal-file-format "%Y-%m-%d.org"
                org-journal-search-results-order-by :desc))

(use-package org-trello
  :ensure t
  :mode ("\\.trello" . org-mode))

(use-package perspective
  :ensure t
  :bind-keymap ("C-z" . perspective-map)
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
  :ensure f
  :delight sphinx-doc-mode
  :hook (python-mode . sphinx-doc-mode))

(use-package undo-tree
  :ensure t
  :delight undo-tree-mode
  :hook (after-init . global-undo-tree-mode)
  :config (setq undo-tree-auto-save-history t
                undo-tree-enable-undo-in-region t
                undo-tree-visualizer-diff t
                undo-tree-visualizer-timestamps t
                undo-tree-history-directory-alist '((".*" . "/home/tmacey/.emacs.d/undo-tree/"))))

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
   '("364e592858d85f3dff9d51af5c72737ca8ef2b76fc60d9d5f0a9995ea927635a" "0f2f1feff73a80556c8c228396d76c1a0342eb4eefd00f881b91e26a14c5b62a" default))
 '(debug-on-error nil)
 '(helm-completion-style 'emacs)
 '(org-trello-current-prefix-keybinding "C-c o" nil (org-trello))
 '(package-selected-packages
   '(hcl-mode lsp-jedi ace-window flycheck-projectile lsp-pyright hybrid-reverse-theme company-quickhelp helm-company editorconfig-generate editorconfig js2-mode dhall-mode yasnippet-snippets xonsh-mode which-key wgrep-ag web-mode use-package undo-tree salt-mode python-docstring py-isort poetry php-mode perspective pallet org-trello org-journal nginx-mode markdown-changelog magit-delta lush-theme lsp-ui linum-relative lice jinja2-mode helm-projectile helm-lsp helm-flx helm-ag github-review git-link git-gutter-fringe+ forge flycheck-mypy fill-column-indicator elpy dockerfile-mode docker delight ag))
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
