;;; package --- Summary
;; Commentary:
;;; This file pulls in the functions and variables from functions.el and
;;; configurations.el as well as providing initialization routines

;;; Code:

;; Redirect Custom to an untracked file so Emacs never writes API keys,
;; theme hashes, or other machine-specific data into the tracked init.el.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file :noerror))

(setenv "LSP_USE_PLISTS" "true")
;; Defer GC during startup for performance; reset to 16MB after init
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 16 1024 1024))))
(add-function :after
                  after-focus-change-function
                  (lambda () (unless (frame-focus-state) (garbage-collect))))
;;; Package manager settings
(setq tls-checktrust t
      gnutls-verify-error t)

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

(setq straight-use-package-by-default t
      straight-disable-compile nil)
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

;; Load user functions early so hook callbacks (e.g. default-minor-modes)
;; are defined before any package loading can trigger prog-mode-hook.
(load-file "~/.emacs.d/functions.el")

(global-auto-revert-mode t)

;; Whitespace: set options before enabling the mode so they take effect on startup.
(use-package whitespace
  :straight nil
  :init
  (setq whitespace-line-column 150
        whitespace-style '(face trailing empty lines-tail tab-mark))
  :hook ((after-init . global-whitespace-mode)
         (markdown-mode . (lambda () (setq-local whitespace-style '(face trailing empty tab-mark))))
         (org-mode . (lambda () (setq-local whitespace-style '(face trailing empty tab-mark))))))

;; http://news.ycombinator.com/item?id=873541
;; (add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'default-minor-modes)

(winner-mode)

(use-package ace-window
  :straight t
  :delight
  :bind ("C-x o" . ace-window)
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
                aw-scope 'frame))

;; Copilot models:
;; curl -s https://api.githubcopilot.com/models \
;;   -H "Authorization: Bearer $OPENAI_API_KEY" \
;;   -H "Content-Type: application/json" \
;;   -H "Copilot-Integration-Id: vscode-chat" | jq -r '.data[].id'
;; openai/gpt-4.1
;; openai/gpt-3.5-turbo
;; openai/gpt-3.5-turbo-0613
;; openai/gpt-4o-mini
;; openai/gpt-4o-mini-2024-07-18
;; openai/gpt-4
;; openai/gpt-4-0613
;; openai/gpt-4o
;; openai/gpt-4o-2024-11-20
;; openai/gpt-4o-2024-05-13
;; openai/gpt-4-o-preview
;; openai/gpt-4o-2024-08-06
;; openai/o3-mini
;; openai/o3-mini-2025-01-31
;; openai/o3-mini-paygo
;; openai/text-embedding-ada-002
;; openai/text-embedding-3-small
;; openai/text-embedding-3-small-inference
;; openai/claude-3.5-sonnet
;; openai/claude-3.7-sonnet
;; openai/claude-3.7-sonnet-thought
;; openai/claude-sonnet-4
;; openai/gemini-2.0-flash-001
;; openai/gemini-2.5-pro
;; openai/gpt-4.1-2025-04-14
(use-package gptel
  :straight t
  :bind (("C-c A" . gptel)
         ("C-c M-A" . gptel-menu))
  :config
  (setq gptel-default-mode 'org-mode)
  ;; Gemini via OpenAI-compatible endpoint
  (gptel-make-gemini "Gemini"
    :key (lambda () (auth-source-pick-first-password :host "generativelanguage.googleapis.com"))
    :stream t)
  ;; Ollama for local models
  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '(gemma3:12b olmo2:13b qwen2.5-coder:14b))
  ;; OpenRouter for model routing
  (gptel-make-openai "OpenRouter"
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :key (lambda () (getenv "OPENROUTER_API_KEY"))
    :stream t
    :models '(google/gemini-2.5-pro anthropic/claude-sonnet-4-5))
  (setq gptel-backend (gptel-get-backend "Gemini")
        gptel-model 'gemini-2.5-pro))

(use-package shell-maker
    :straight (:type git :host github :repo "xenodium/shell-maker" :files ("*.el")))

(use-package agent-shell
  :straight t
  :ensure-system-package
  ((copilot-cli  . "npm install -g @github/copilot")
   (gemini-cli   . "npm install -g @google/gemini-cli")
   (kilocode-cli . "npm install -g @kilocode/cli")))

(use-package ag
  :straight t
  :defer t)

(use-package all-the-icons
  :straight t)

;; Combobulate with tresitter for structured editing
(use-package treesit
  :straight nil
  :mode (("\\.tsx\\'" . tsx-ts-mode))
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             ;; Note the version numbers. These are the versions that
             ;; are known to work with Combobulate *and* Emacs.
             '(
               (bash . ("https://github.com/tree-sitter/tree-sitter-bash" "v0.25.0"))
               (c . ("https://github.com/tree-sitter/tree-sitter-c" "v0.24.1"))
               ;; (c++ . ("https://github.com/tree-sitter/tree-sitter-cpp" "v0.23.4"))
               (css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.23.2"))
               (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.23.4"))
               ;; (haskell . ("https://github.com/tree-sitter/tree-sitter-haskell" "v0.23.1"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.23.2"))
               (java . ("https://github.com/tree-sitter/tree-sitter-java" "v0.23.5"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.23.1"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.24.8"))
               ;; (julia . ("https://github.com/tree-sitter/tree-sitter-julia" "v0.23.1"))
               ;; (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
               ;; (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" "v0.24.2"))
               ;; (php . ("https://github.com/tree-sitter/tree-sitter-php" "v0.24.2"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.23.6"))
               (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby" "v0.23.1"))
               (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.24.0"))
               ;; (scala . ("https://github.com/tree-sitter/tree-sitter-scala" "v0.24.0"))
               ;; (swift . ("https://github.com/tree-sitter/swift-tree-sitter" "0.9.0"))
               (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src"))
               ;; (verilog . ("https://github.com/tree-sitter/tree-sitter-verilog" "v1.0.3"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
               )
             )
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional. Combobulate works in both xxxx-ts-modes and
  ;; non-ts-modes.

  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '(
             (bash-mode . bash-ts-mode)
             (c++-mode . c++-ts-mode)
             (c-mode . c-ts-mode)
             (conf-toml-mode . toml-ts-mode)
             (css-mode . css-ts-mode)
             (css-mode . css-ts-mode)
             (go-mode . go-ts-mode)
             (java-mode . java-ts-mode)
             (js-json-mode . json-ts-mode)
             (js2-mode . js-ts-mode)
             (json-mode . json-ts-mode)
             (php-mode . php-ts-mode)
             (python-mode . python-ts-mode)
             (ruby-mode . ruby-ts-mode)
             (sh-mode . bash-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (yaml-mode . yaml-ts-mode)
             )
           )
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  ;; Auto-install any missing grammars at startup. Safe to run repeatedly
  ;; because mp-setup-install-grammars checks treesit-language-available-p.
  ;; To upgrade grammars manually: M-x mp-setup-install-grammars
  (mp-setup-install-grammars)
  ;; Do not forget to customize Combobulate to your liking:
  ;;
  ;;  M-x customize-group RET combobulate RET
  ;;
  (use-package combobulate
    :straight (:host github :repo "mickeynp/combobulate" :files ("combobulate*.el"))
    ;; :custom
    ;; You can customize Combobulate's key prefix here.
    ;; Note that you may have to restart Emacs for this to take effect!
    :bind-keymap ("C-c o" . combobulate-key-map)
    :hook ((prog-mode . combobulate-mode))))

;; claude-code-ide provides MCP-based bidirectional integration (Claude
;; can call LSP, tree-sitter, project commands from within Emacs).
;; Requires an active Claude Code (claude.ai/code) subscription.
;; (use-package claude-code-ide
;;   :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
;;   :bind ("C-c C-'" . claude-code-ide-menu)
;;   :config
;;   (claude-code-ide-emacs-tools-setup))

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

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode))

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))  ;; if you don't use "straight", install otherwise
  :ensure t
  ;; :hook (prog-mode . copilot-mode)
  :bind (
         ("C-c <tab>" . copilot-accept-completion)
         ("C-c C-<tab>" . copilot-accept-completion-by-word)
         ("C-c M-<tab>" . copilot-accept-completion-by-line)
         ("C-c C-g" . copilot-clear-overlay)
         ("C-c C-n" . copilot-next-completion)
         ("C-c C-p" . copilot-previous-completion)
         )
  :config (setq copilot-idle-delay 5)
  )

(use-package copilot-chat
  :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :after (request org markdown-mode shell-maker)
  :hook (git-commit-setup-hook . copilot-chat-insert-commit-message)
)

(use-package csv-mode
  :straight t)

(use-package delight
  :straight t
  :config
  (delight '((global-whitespace-mode nil "whitespace")
             (subword-mode nil "subword")
             (flyspell-mode nil "flyspell"))))

(use-package dockerfile-mode
  :straight t)

(use-package dumb-jump
  :straight t)

(use-package editorconfig
  :straight t
  :delight
  :hook (prog-mode . editorconfig-mode))

(use-package editorconfig-generate
  :straight t)

(use-package ef-themes
  :straight t
  :init
  ;; This makes the Modus commands listed below consider only the Ef
  ;; themes.  For an alternative that includes Modus and all
  ;; derivative themes (like Ef), enable the
  ;; `modus-themes-include-derivatives-mode' instead.
  (ef-themes-take-over-modus-themes-mode 1)
  :bind
  (("<f5>" . modus-themes-rotate)
   ("C-<f5>" . modus-themes-select)
   ("M-<f5>" . modus-themes-load-random))
  :config
  ;; All customisations here.
  (setq modus-themes-mixed-fonts t
        modus-themes-italic-constructs t
        modus-themes-variable-pitch-ui t
        modus-themes-to-toggle '(ef-symbiosis ef-trio-light))

  ;; Finally, load your theme of choice (or a random one with
  ;; `modus-themes-load-random', `modus-themes-load-random-dark',
  ;; `modus-themes-load-random-light').
  (modus-themes-load-theme 'ef-symbiosis))

(use-package ellama
  :straight t
  :bind ("C-c e" . ellama-transient-main-menu)
  ;; send last message in chat buffer with C-c C-c
  :hook (org-ctrl-c-ctrl-c-final . ellama-chat-send-last-message)
  :init
  ;; setup key bindings
  ;; (setopt ellama-keymap-prefix "C-c e")
  ;; language you want ellama to translate to
  (setopt ellama-language "English")
  ;; could be llm-openai for example
  (require 'llm-ollama)
  (setopt ellama-provider
          (make-llm-ollama
           ;; this model should be pulled to use it
           ;; value should be the same as you print in terminal during pull
           :chat-model "olmo2:13b"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("num_ctx" . 8192))))
  (setopt ellama-summarization-provider
          (make-llm-ollama
           :chat-model "gemma3:12b"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("num_ctx" . 32768))))
  (setopt ellama-coding-provider
          (make-llm-ollama
           :chat-model "gemma3:12b"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("num_ctx" . 32768))))
  ;; Predefined llm providers for interactive switching.
  ;; You shouldn't add ollama providers here - it can be selected interactively
  ;; without it. It is just example.
  ;; (setopt ellama-providers
  ;;         '(("zephyr" . (make-llm-ollama
  ;;                        :chat-model "zephyr:7b-beta-q6_K"
  ;;                        :embedding-model "zephyr:7b-beta-q6_K"))
  ;;           ("mistral" . (make-llm-ollama
  ;;                         :chat-model "mistral:7b-instruct-v0.2-q6_K"
  ;;                         :embedding-model "mistral:7b-instruct-v0.2-q6_K"))
  ;;           ("mixtral" . (make-llm-ollama
  ;;                         :chat-model "mixtral:8x7b-instruct-v0.1-q3_K_M-4k"
  ;;                         :embedding-model "mixtral:8x7b-instruct-v0.1-q3_K_M-4k"))))
  ;; Naming new sessions with llm
  (setopt ellama-naming-provider
          (make-llm-ollama
           :chat-model "llama3.2"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("stop" . ("\n")))))
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
  ;; Translation llm provider
  (setopt ellama-translation-provider
          (make-llm-ollama
           :chat-model "qwen2.5:14b"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params
           '(("num_ctx" . 32768))))
  (setopt ellama-extraction-provider (make-llm-ollama
                                      :chat-model "qwen2.5-coder:14b"
                                      :embedding-model "nomic-embed-text"
                                      :default-chat-non-standard-params
                                      '(("num_ctx" . 32768))))
  ;; customize display buffer behaviour
  ;; see ~(info "(elisp) Buffer Display Action Functions")~
  (setopt ellama-chat-display-action-function #'display-buffer-full-frame)
  (setopt ellama-instant-display-action-function #'display-buffer-at-bottom)
  :config
  ;; show ellama context in header line in all buffers
  (ellama-context-header-line-global-mode +1))

(use-package flycheck
  :straight t
  :delight flycheck-mode
  :hook (after-init . global-flycheck-mode)
  :init
  ;; (use-package flycheck-mypy :straight t)
  :config (setq flycheck-disabled-checkers '(python-pycompile python-pylint)))
                ;; flycheck-python-mypy-config "pyproject.toml"))

(use-package flycheck-projectile
  :straight t)

(use-package forge
  :straight t
  :after magit)

(use-package git-link
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

(use-package lsp-mode
  :straight t
  :delight
  :hook
;;  (python-mode . (lambda ()
;;                   (let ((project-root (projectile-project-root)))
;;                     (when project-root
;;                       (setq lsp-ruff-python-path (concat project-root "/.venv/bin/python")
;;                             lsp-ruff-ruff-args '("--verbose" "--preview" (concat "--config " project-root "/pyproject.toml")))))))
  ;; (dockerfile-mode . lsp-deferred)
  ;; (go-mode . lsp-deferred)
  ;; (go-ts-mode . lsp-deferred)
  ;; (html-mode . lsp-deferred)
  ;; (js-mode . lsp-deferred)
  ;; (js-ts-mode . lsp-deferred)
  ;; (json-mode . lsp-deferred)
  ;; (json-ts-mode . lsp-deferred)
  ;; (php-mode . lsp-deferred)
  ;; (php-ts-mode . lsp-deferred)
  ;; (python-mode . lsp-deferred)
  ;; (python-ts-mode . lsp-deferred)
  ;; (shell-mode . lsp-deferred)
  ;; (web-mode . lsp-deferred)
  (prog-mode . lsp-deferred)
  :bind
  (:map lsp-mode-map)
  ("M-." . xref-find-definitions)
  :bind-keymap ("C-;" . lsp-command-map)
  :init
  (setq lsp-keymap-prefix "C-;"
        lsp-auto-configure t
        lsp-auto-register-remote-clients nil
        lsp-before-save-edits nil
        lsp-copilot-enabled nil
        lsp-diagnostic-package :none
        lsp-enable-completion-at-point t
        lsp-enable-file-watchers t
        lsp-enable-folding t
        lsp-enable-imenu t
        lsp-auto-guess-root t
        lsp-eldoc-render-all t
        lsp-enable-indentation t
        lsp-enable-semantic-highlighting t
        lsp-enable-text-document-color t
        lsp-file-watch-threshold 15000
        lsp-idle-delay 2
        lsp-imenu-show-container-name t
        lsp-keep-workspace-alive nil
        lsp-log-io nil
        lsp-modeline-code-actions-mode t
        lsp-prefer-capf t
        lsp-ruff-log-level "warn"
        lsp-ruff-show-notifications "always"
        lsp-use-plists t
        lsp-semantic-tokens-enable t
        read-process-output-max (* 1024 1024))
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.?venv\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.?undo-tree\\'")
  ;; Sourcery LSP add-on for Python
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("sourcery" "lsp"))
                    :initialization-options `((token . ,(auth-source-pick-first-password :host "sourcery.ai"))
                                              (extension_version . "emacs-lsp")
                                              (editor_version . "emacs"))
                    :activation-fn (lsp-activate-on "python")
                    :server-id 'sourcery
                    :add-on? t))
  ;; Pyrefly LSP client for Python type checking
  (defgroup lsp-python-refly nil
    "LSP support for Python (pyrefly)."
    :group 'lsp-mode
    :link '(url-link "https://pyrefly.org"))
  (defcustom lsp-python-refly-clients-server-command '("pyrefly" "lsp" "-v")
    "Command to start the python pyrefly language server."
    :group 'lsp-python-refly
    :risky t
    :type '(repeat string))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection (lambda () lsp-python-refly-clients-server-command))
                    :activation-fn (lsp-activate-on "python")
                    :priority -1
                    :add-on? t
                    :server-id 'py-refly))
  :commands (lsp lsp-deferred))

;; (use-package lsp-pyright
;;   :straight t
;;   :custom (lsp-pyright-langserver-command "basedpyright") ;; or pyright
;;   :config (setq lsp-pyright-diagnostic-mode "workspace"
;;                 lsp-pyright-use-library-code-for-types t
;;                 lsp-pyright-auto-import-completions t
;;                 lsp-pyright-disable-organize-imports t
;;                 lsp-pyright-langserver-command "basedpyright"
;;                 lsp-pyright-typechecking-mode "off"))

(use-package lsp-java
  :straight t)

; (use-package lsp-treemacs
;   :straight t
;   :config (setq lsp-treemacs-sync-mode 1)
;   )

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
  :demand t  ; Load magit immediately to avoid autoload issues
  :bind ("C-x g" . magit-status)
  :config (setq magit-commit-arguments '("-S")
                magit-log-section-arguments '("-n256" "--decorate")))

(use-package magit-delta
  :straight t
  :after magit
  :hook (magit-mode . magit-delta-mode))

(use-package markdown-changelog
  :straight t)

(use-package org
  :straight t
  :bind (("C-c L" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c s e" . org-edit-src-code))
         ;; ("C-c o n" lambda () (interactive) (find-file (concat org-directory "notes/")))
         ;; ("C-c o t" lambda () (interactive) (find-file (concat org-directory "todo/")))
         ;; ("C-c o T" lambda () (interactive) (find-file (concat org-directory "trello/"))))
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
           "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")))
  (require 'ox-latex)
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("" "color")))

(use-package org-journal
  :straight t
  :config (setq org-journal-date-format "%Y-%m-%d"
                org-journal-dir "~/Dropbox/org/journal/"
                org-journal-file-format "%Y-%m-%d.org"
                org-journal-search-results-order-by :desc))

;; (use-package org-trello
;;   :straight t
;;   :mode ("\\.trello" . org-mode))

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
  (persp-mode))
  ;; (run-with-timer 300 (* 5 60) 'persp-state-save "~/.emacs.d/perspective_state.el"))

(use-package pet
  :straight t
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10)
(pet-def-config-accessor pre-commit-config
                         :file-name ".no-such-file.yaml"
                         :parser pet-parse-config-file))

(use-package php-mode
  :straight t)

(use-package projectile
  :straight t
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (:map projectile-command-map
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
  :hook (python-base-mode . python-docstring-mode))

(use-package sops
  :straight (:type git :host github :repo "djgoku/sops")
  :bind (:map sops-mode-map
         ("C-c C-c" . sops-save-file)
         ("C-c C-k" . sops-cancel)
         ("C-c C-d" . sops-edit-file))
  :init
  (global-sops-mode 1))

(use-package sphinx-doc
  :straight t
  :delight sphinx-doc-mode
  :hook (python-base-mode . sphinx-doc-mode))

(use-package sphinx-mode
  :straight t
  :delight sphinx-mode
  :hook (python-base-mode . sphinx-mode))


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

(use-package unicode-fonts
  :straight t
  :config
  (unicode-fonts-setup))

(use-package vcl-mode
  :straight t)

(use-package vterm
  :straight t
  :config (setq vterm-max-scrollback 10000))

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

(use-package yaml-mode
  :straight t
  :hook ((yaml-mode . (lambda () (setq-local electric-indent-mode nil)))
         ;; yaml-ts-mode is a remap target; hooks do not carry over from yaml-mode.
         (yaml-ts-mode . (lambda () (setq-local electric-indent-inhibit t)))))

(use-package wdired
  :straight nil
  :custom (wdired-allow-to-change-permissions t))

(load-file "~/.emacs.d/configurations.el")

;; Values previously managed by Custom — now explicit to keep init.el clean.
(setq horizontal-scroll-bar-mode nil)
(url-handler-mode 1)

(set-cursor-color "white")

(provide 'init)
;;; init.el ends here
