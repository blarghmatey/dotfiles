;;; package --- Summary
;; Commentary:
;;; This file pulls in the functions and variables from functions.el and
;;; configurations.el as well as providing initialization routines

;;; Code:
(setenv "LSP_USE_PLISTS" "true")
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

(setq package-enable-at-startup nil)
(setq straight-check-for-modifications nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)
;; (require 'package)
;; (setq package-archives
;;       '(("gnu" . "https://elpa.gnu.org/packages/")
;;         ("melpa" . "https://melpa.org/packages/")))

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
;; (require 'use-package)
;; (require 'cask)
;; (cask-initialize)

;; (setq load-path (append load-path '("~/.emacs.d/plugins")))

(require 'epg)
(setq epg-pinentry-mode 'loopback)
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
  :straight t
  :delight
  :bind ("C-x o" . ace-window)
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
                aw-scope 'frame))

(use-package aider
  :straight (:host github :repo "tninja/aider.el" :files ("aider.el"))
  :config
  (setq aider-args '("--model" "ollama_chat/qwen2.5-coder"))
  (setenv "OLLAMA_API_BASE" "http://127.0.0.1:11434")
  ;; Or use chatgpt model since it is most well known
  ;; (setq aider-args '("--model" "gpt-4o-mini"))
  ;; (setenv "OPENAI_API_KEY" <your-openai-api-key>)
  ;; ;;
  ;; Optional: Set a key binding for the transient menu
  (global-set-key (kbd "C-c A") 'aider-transient-menu))

;; Optional helm support
(use-package aider-helm
  :straight (:host github :repo "tninja/aider.el" :files ("aider-helm.el"))
  :after (aider helm))

(use-package ag
  :straight t
  :defer t)

(use-package all-the-icons
  :straight t)

;; we recommend using use-package to organize your init.el
;; (use-package codeium
;;     ;; if you use straight
;;     :straight '(:type git :host github :repo "Exafunction/codeium.el")
;;     ;; otherwise, make sure that the codeium.el file is on load-path

;;     :init
;;     ;; use globally
;;     (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
;;     ;; or on a hook
;;     ;; (add-hook 'python-mode-hook
;;     ;;     (lambda ()
;;     ;;         (setq-local completion-at-point-functions '(codeium-completion-at-point))))

;;     ;; if you want multiple completion backends, use cape (https://github.com/minad/cape):
;;     ;; (add-hook 'python-mode-hook
;;     ;;     (lambda ()
;;     ;;         (setq-local completion-at-point-functions
;;     ;;             (list (cape-super-capf #'codeium-completion-at-point #'lsp-completion-at-point)))))
;;     ;; an async company-backend is coming soon!

;;     ;; codeium-completion-at-point is autoloaded, but you can
;;     ;; optionally set a timer, which might speed up things as the
;;     ;; codeium local language server takes ~0.2s to start up
;;     ;; (add-hook 'emacs-startup-hook
;;     ;;  (lambda () (run-with-timer 0.1 nil #'codeium-init)))

;;     ;; :defer t ;; lazy loading, if you want
;;     :config
;;     (setq use-dialog-box nil) ;; do not use popup boxes

;;     ;; if you don't want to use customize to save the api-key
;;     ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

;;     ;; get codeium status in the modeline
;;     (setq codeium-mode-line-enable
;;         (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
;;     (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
;;     ;; alternatively for a more extensive mode-line
;;     ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

;;     ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
;;     (setq codeium-api-enabled
;;         (lambda (api)
;;             (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
;;     ;; you can also set a config for a single buffer like this:
;;     ;; (add-hook 'python-mode-hook
;;     ;;     (lambda ()
;;     ;;         (setq-local codeium/editor_options/tab_size 4)))

;;     ;; You can overwrite all the codeium configs!
;;     ;; for example, we recommend limiting the string sent to codeium for better performance
;;     (defun my-codeium/document/text ()
;;         (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
;;     ;; if you change the text, you should also change the cursor_offset
;;     ;; warning: this is measured by UTF-8 encoded bytes
;;     (defun my-codeium/document/cursor_offset ()
;;         (codeium-utf8-byte-length
;;             (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
;;     (setq codeium/document/text 'my-codeium/document/text)
;;     (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))

(use-package company
  :straight t
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
  :straight t)

(use-package csv-mode
  :straight t)

(use-package delight
  :straight t)

(use-package docker
  :straight t
  :bind ("C-c d" . docker))

(use-package dockerfile-mode
  :straight t)

(use-package dumb-jump
  :straight t)

(use-package earthfile-mode
  :straight t)

(use-package editorconfig
  :straight t
  :delight
  :hook (prog-mode . editorconfig-mode))

(use-package editorconfig-generate
  :straight t)

(use-package ef-themes
  :straight t
  :config (setq ef-themes-mixed-fonts t
                ef-themes-variable-pitch-ui t
                ef-themes-to-toggle '(ef-symbiosis ef-trio-light))
  :init (ef-themes-select 'ef-symbiosis))

;; Emacs IPython/Jupyter Notebooks
(use-package ein
  :straight t)

;; YOU DON'T NEED NONE OF THIS CODE FOR SIMPLE INSTALL
;; IT IS AN EXAMPLE OF CUSTOMIZATION.
(use-package ellama
  :straight t
  :init
  ;; setup key bindings
  (setopt ellama-keymap-prefix "C-c ;")
  ;; language you want ellama to translate to
  ;; could be llm-openai for example
  (require 'llm-ollama)
  ;; (setopt ellama-provider
  ;;           (make-llm-ollama
  ;;            ;; this model should be pulled to use it
  ;;            ;; value should be the same as you print in terminal during pull
  ;;            :chat-model "mistral:7b-instruct-v0.2-q6_K"
  ;;            :embedding-model "mistral:7b-instruct-v0.2-q6_K"))
  ;; Naming new sessions with llm
  ;; (setopt ellama-naming-provider
  ;;       (make-llm-ollama
  ;;        :chat-model "mistral:7b-instruct-v0.2-q6_K"
  ;;        :embedding-model "mistral:7b-instruct-v0.2-q6_K"))
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
  ;; Translation llm provider
  ;; (setopt ellama-translation-provider (make-llm-ollama
  ;;                    :chat-model "sskostyaev/openchat:8k"
  ;;                    :embedding-model "nomic-embed-text"))
  ;; Define the providers we like
  (setq-default ellama-providers
        (list
         (cons "chat-semantic" (make-llm-ollama
                    :chat-model "llama3.1"
                    :embedding-model "llama3.1"))
         (cons "chat-literal" (make-llm-ollama
                       :chat-model "mistral"
                       :embedding-model "mistral"))
         (cons "code-completion" (make-llm-ollama
                      :chat-model "codellama"
                      :embedding-model "codellama"))))

  ;; Now set specific providers from the list by name
  (setq-default ellama-provider (alist-get "chat-semantic" ellama-providers
                       nil nil 'string-equal))
                    ; Code completion should go in
                    ; here once supported
  (setq-default ellama-naming-provider (alist-get "chat-literal" ellama-providers
                          nil nil 'string-equal))

  ;; Convenience functions for mode change events
  (defun my-select-code-completion ()
    (setq ellama-provider (alist-get "code-completion"
                     ellama-providers nil nil 'string-equal)))
  (defun my-select-chat ()
    (setq ellama-provider (alist-get "chat-semantic"
                     ellama-providers nil nil 'string-equal)))
  
  ;; This ought to use :hook, but start by getting it working at all
  (add-hook 'prog-mode-hook  (lambda () 'my-select-code-completion))
  (add-hook 'text-mode-hook  (lambda () 'my-select-chat))
  )

(use-package elpy
  :straight t
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
                       elpy-module-highlight-indentation)
        elpy-test-runner 'elpy-test-pytest-runner)
  (elpy-enable))

(use-package fill-column-indicator
  :straight t
  :delight fci-mode
  :hook (python-mode . fci-mode)
  :config (setq fill-column 88
                fci-rule-column 88
                fci-rule-color "#37474f"))

(use-package flycheck
  :straight t
  :delight flycheck-mode
  :hook (after-init . global-flycheck-mode)
  :init
  (use-package flycheck-mypy :straight t)
  :config (setq flycheck-disabled-checkers '(python-pycompile python-pylint)
                flycheck-python-mypy-config "pyproject.toml"))

(use-package flycheck-projectile
  :straight t)

(use-package forge
  :straight t
  :after magit
  :config (setq forge-topic-list-limit 0))

(use-package git-link
  :straight t)

(use-package github-review
  :straight t)

(use-package go-mode
  :straight t)

(use-package hcl-mode
  :straight t)

(use-package helm
 :straight t
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
       helm-move-to-line-cycle-in-source nil
       completion-styles `(basic partial-completion emacs22 initials))
 (helm-autoresize-mode 1)
 :bind (("M-x" . helm-M-x)
        ("C-x C-f" . helm-find-files)
        ("C-x b" . helm-buffers-list)))

(use-package helm-ag
 :straight t
 :after (:all helm ag))

(use-package helm-company
 :straight t)

(use-package helm-lsp
 :straight t
 :commands helm-lsp-workspace-symbol)

(use-package helm-projectile
 :straight t
 :after (:all projectile helm)
 :config
 (helm-projectile-on)
 (setq projectile-mode-line-prefix ""))

(use-package indent-control
  :straight t)

(use-package jinja2-mode
  :straight t
  :mode (("\\.j2" . jinja2-mode)
         ("\\.jinja" . jinja2-mode)))

(use-package json-mode
  :straight t)

(use-package k8s-mode
 :straight t
 :config
 (setq k8s-search-documentation-browser-function 'browse-url-firefox
       k8s-site-docs-version "v1.22"
       k8s-site-docs-url "https://kubernetes.io/docs/reference/generated/kubernetes-api/")
 :hook (k8s-mode . yas-minor-mode))

(use-package kubed
  :straight t
  :bind-keymap ("C-c K" . kubed-prefix-map))

(use-package lice
  :straight t)

(use-package linum-relative
  :straight t
  :bind ("C-c t l" . linum-relative-toggle))

(use-package lsp-mode
  :straight t
  :delight
  :hook
  (js-mode . lsp-deferred)
  (json-mode . lsp-deferred)
  (shell-mode . lsp-deferred)
  (html-mode . lsp-deferred)
  (web-mode . lsp-deferred)
  (python-mode . (lambda ()
                   (let ((project-root (projectile-project-root)))
                     (when project-root
                       (setq lsp-ruff-python-path (concat project-root "/.venv/bin/python")
                             lsp-ruff-ruff-args ("--preview" (concat "--config " project-root "/pyproject.toml")))))))
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
        lsp-copilot-enabled t
        lsp-diagnostic-package :none
        lsp-enable-completion-at-point t
        lsp-enable-file-watchers t
        lsp-enable-folding t
        lsp-enable-imenu t
        lsp-enable-indentation t
        lsp-enable-semantic-highlighting t
        lsp-enable-text-document-color t
        lsp-file-watch-threshold 15000
        lsp-imenu-show-container-name t
        lsp-keep-workspace-alive nil
        lsp-log-io t
        lsp-modeline-code-actions-mode t
        lsp-prefer-capf t
        ;; lsp-ruff-ruff-args '("--preview")
        lsp-ruff-import-strategy "fromEnvironment"
        lsp-use-plists t
        lsp-semantic-tokens-enable t
        read-process-output-max (* 1024 1024))
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.?venv\\'")
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.?undo-tree\\'")
    )
  :config (advice-add 'lsp :before
                      (lambda (&rest _args)
                        (eval
                         '(setf
                           (lsp-session-server-id->folders (lsp-session))
                           (ht)))))
  :commands (lsp lsp-deferred))

(use-package lsp-pyright
  :straight t
  :custom (lsp-pyright-langserver-command "basedpyright") ;; or pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp)))
  :config (setq lsp-pyright-diagnostic-mode "workspace"
                lsp-pyright-use-library-code-for-types t
                lsp-pyright-auto-import-completions t
                lsp-pyright-disable-organize-imports t
                lsp-pyright-langserver-command "basedpyright"
                lsp-pyright-typechecking-mode "off"))

(use-package lsp-treemacs
  :straight t
  :config (setq lsp-treemacs-sync-mode 1)
  )

(use-package lsp-ui
  :straight t
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
  :straight t
  :bind ("C-x g" . magit-status)
  :config (setq magit-last-seen-setup-instructions "1.4.0"
                magit-commit-arguments '("-S")
                magit-log-section-arguments '("-n256" "--decorate")))

(use-package magit-delta
  :straight t)

(use-package markdown-changelog
  :straight t)

(use-package mmm-mode
  :straight t
  :delight)

(use-package nginx-mode
  :straight t)

(use-package obsidian
  :straight t
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
  :straight t
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
  :straight t
  :config (setq org-journal-date-format "%Y-%m-%d"
                org-journal-dir "~/Dropbox/org/journal/"
                org-journal-file-format "%Y-%m-%d.org"
                org-journal-search-results-order-by :desc))

;; (use-package org-trello
;;   :straight t
;;   :mode ("\\.trello" . org-mode))

(use-package ox-hugo
  :straight t)

(use-package perspective
  :straight t
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
  :straight t)

(use-package poetry
  :straight t
  :hook
  (python-mode . poetry-tracking-mode)
  :config (setq poetry-tracking-strategy 'projectile))

(use-package projectile
  :straight t
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (("M-P" . projectile-find-file)
         :map projectile-command-map
         ("p" . projectile-switch-project)
         ("C-c" . flycheck-projectile-list-errors))
  :init (setq projectile-completion-system 'helm)
  :hook (after-init . projectile-mode))

(use-package python-insert-docstring
  :straight t
  :init (setq python-docstring-field-no-arg-re "^\\s-*\\([@:]\\)\\(raise\\|raises\\|return\\|returns\\|rtype\\|returntype\\|type\\|sort\\|see\\|seealso\\|note\\|attention\\|bug\\|warning\\|warn\\|version\\|todo\\|deprecated\\|since\\|status\\|change\\|changed\\|permission\\|requires\\|require\\|requirement\\|precondition\\|precond\\|postcondition\\|postcod\\|invariant\\|author\\|organization\\|org\\|copyright\\|(c)\\|license\\|contact\\|summary\\|params\\|param\\|yield\\|yields\\)\\(:\\)"))

(use-package python-docstring
  :straight t
  :delight
  :hook (python-mode . python-docstring-mode))

(use-package pyvenv
  :straight t
  :config
  (pyvenv-mode 1))

(use-package salt-mode
  :straight t)

(use-package sops
  :straight (:type git :host github :repo "djgoku/sops")
  :bind (("C-c C-c" . sops-save-file)
         ("C-c C-k" . sops-cancel)
         ("C-c C-d" . sops-edit-file))
  :init
  (global-sops-mode 1))

(use-package sphinx-doc
  :straight t
  :delight sphinx-doc-mode
  :hook (python-mode . sphinx-doc-mode))

(use-package sphinx-mode
  :straight t
  :delight sphinx-mode
  :hook (python-mode . sphinx-mode))


;; (use-package tabnine
;;   :commands (tabnine-start-process)
;;   :hook (prog-mode . tabnine-mode)
;;   :straight t
;;   :diminish "⌬"
;;   :custom
;;   (tabnine-wait 10)
;;   (tabnine-minimum-prefix-length 0)
;;   :hook (kill-emacs . tabnine-kill-process)
;;   :config
;;   (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point)
;;   (setq tabnine-idle-delay 1)
;;   (tabnine-start-process)
;;   :bind
;;   (:map  tabnine-completion-map
;;          ("C-<tab>" . tabnine-accept-completion)
;;          ("C-TAB" . tabnine-accept-completion)
;;          ("C-M-f" . tabnine-accept-completion-by-word)
;;          ("C-M-<return>" . tabnine-accept-completion-by-line)
;;          ("C-g" . tabnine-clear-overlay)
;;          ("M-[" . tabnine-previous-completion)
;;          ("M-]" . tabnine-next-completion)))

(use-package treemacs
  :straight t
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
  :straight t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :straight t)

(use-package treemacs-magit
  :after (treemacs magit)
  :straight t)

(use-package treemacs-perspective ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs perspective) ;;or perspective vs. persp-mode
  :straight t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package typescript-mode
  :straight t)

(use-package undo-tree
  :straight t
  :delight undo-tree-mode
  :hook (after-init . global-undo-tree-mode)
  :config (setq undo-tree-auto-save-history t
                undo-tree-enable-undo-in-region t
                undo-tree-visualizer-diff t
                undo-tree-visualizer-timestamps t
                undo-tree-history-directory-alist '(("." . "/home/tmacey/.emacs.d/undo-tree/"))))

(use-package lush-theme
  :straight t)

(use-package vcl-mode
  :straight t)

(use-package web-mode
  :straight t
  :mode ("\\.html" . web-mode))

(use-package wgrep-ag
  :straight t)

(use-package which-key
  :straight t
  :demand t
  :delight which-key-mode
  :init
  (which-key-setup-side-window-bottom)
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'bottom)
  :config (which-key-mode)
  :commands which-key-mode)

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
 '(codeium/metadata/api_key "c4716906-af39-45c3-8c42-c71afc182c7c")
 '(custom-safe-themes
   '("9b7d703afa15e320af9aa6b3e02bb1ebb8657c23043f7054357f86ce014b5461" "0f964c8dbc5a668cc2ba7aa1003792fbbf3000a6ed69c0e53b1eeb2c1afc25cb" "b87f0a7cc94fc07f1417f95de2382a7c1c853a6822d987af45b3b3c5e95e3abb" "9f97708991e9b0ddc2d428e5bae87d97d8b6c6c09ef82cbfa26a797560de7cec" "6abef8c5e70ae252c41e9c91a885635de66816204a0bd9102387f6f7c419a7a5" "2480d2400cf9eb2f58703d0f3e6ae23b15d8bc6c7b8070c65328f32f31df6a03" "a2c06295bcca9ffc56f22b4d1f1f145cf9a6953785099199afe2e4db75e54630" "448175cf1da8bdb6dd310a33283789bdb5b76402adcc0a23bcebeb3e6f667bdf" "13f4cc4607ec8c2aada98ee92293547d7134ffa4d746c9104647461bcbcab8a7" "cda446bf884e720497d5f1463ff699896c5de42352321c7bc91e637da25cad2e" "dd64cf49a64a5adeebc30a8e968db73f549e11d6adcd1318893fded4ee0b214b" "0c2d7f410f835d59a0293f2a55744e9d3be13aab8753705c6ad4a9a968fb3b28" "364e592858d85f3dff9d51af5c72737ca8ef2b76fc60d9d5f0a9995ea927635a" "0f2f1feff73a80556c8c228396d76c1a0342eb4eefd00f881b91e26a14c5b62a" default))
 '(debug-on-error nil)
 '(elpy-syntax-check-command "ruff check")
 '(grep-command "ag")
 '(helm-completion-style 'emacs)
 '(horizontal-scroll-bar-mode nil)
 '(org-trello-current-prefix-keybinding "C-c o")
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
