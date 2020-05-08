;;; package --- Summary
;; Commentary:
;;; This file pulls in the functions and variables from functions.el and
;;; configurations.el as well as providing initialization routines

;;; Code:

;;; Package manager settings
;; (setq tls-checktrust t)
;; (let ((trustfile
;;        (replace-regexp-in-string
;;         "\\\\" "/"
;;         (replace-regexp-in-string
;;          "\n" ""
;;          (shell-command-to-string "python -m certifi")))))
;;   (setq tls-program
;;         (list
;;          (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
;;                  (if (eq window-system 'w32) ".exe" "") trustfile)))
;;   (setq gnutls-verify-error t)
;;   (setq gnutls-trustfiles (list trustfile)))

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

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

(use-package ag
  :ensure t
  :defer t)

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :bind ("C-." . company-complete)
  :config (setq company-idle-delay 2
                company-show-numbers t
                company-auto-complete nil
                company-minimum-prefix-length 1
                company-auto-complete-chars (quote (32 95 40 46))))

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package dockerfile-mode
  :ensure t)

(use-package elpy
  :ensure t
  :defer t
  :after python
  :config (elpy-enable))

(use-package elscreen
  :ensure t
  :hook
  (kill-emacs . elscreen-store)
  (after-init . elscreen-start)
  :init
  (defvar emacs-configuration-directory
    "~/.emacs.d/"
    "The directory where the emacs configuration files are stored.")

  (defvar elscreen-tab-configuration-store-filename
    (concat emacs-configuration-directory ".elscreen")
    "The file where the elscreen tab configuration is stored.")

  (defun elscreen-store ()
    "Store the elscreen tab configuration."
    (interactive)
    (if (desktop-save (concat emacs-configuration-directory "elscreen-desktop") t)
        (with-temp-file elscreen-tab-configuration-store-filename
            (insert (prin1-to-string (elscreen-get-screen-to-name-alist))))))

  (defun elscreen-restore ()
    "Restore the elscreen tab configuration."
    (interactive)
    (if (desktop-read (concat emacs-configuration-directory "elscreen-desktop"))
        (let ((screens (reverse
                        (read
                         (with-temp-buffer
                           (insert-file-contents elscreen-tab-configuration-store-filename)
                           (buffer-string))))))
          (while screens
            (setq screen (car (car screens)))
            (setq buffers (split-string (cdr (car screens)) ":"))
            (if (eq screen 0)
                (switch-to-buffer (car buffers))
              (elscreen-find-and-goto-by-buffer (car buffers) t t))
            (while (cdr buffers)
              (switch-to-buffer-other-window (car (cdr buffers)))
              (setq buffers (cdr buffers)))
            (setq screens (cdr screens))))))
  :config
  (run-with-timer 300 (* 5 60) 'elscreen-store))

(use-package fill-column-indicator
  :ensure t
  :hook (python-mode . fci-mode)
  :config (setq fci-rule-column 120
                fci-rule-color "#37474f"))

(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode)
  :init
  (use-package flycheck-mypy :ensure t)
  (use-package flycheck-pyflakes :ensure t)
  :config (setq flycheck-flake8-maximum-complexity 15
                flycheck-flake8rc "setup.cfg"))

(use-package forge
  :ensure t
  :after magit)

(use-package git-gutter-fringe+
  :ensure t
  :hook (after-init . global-git-gutter+-mode))

(use-package git-link
  :ensure t)

(use-package helm
  :ensure t
  :config
  (setq helm-mode-fuzzy-match t
        helm-completion-in-region-fuzzy-match t
        helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-completion-style 'emacs
        completion-styles '(helm-flex))
  (helm-autoresize-mode 1)
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)))

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

(use-package helm-projectile
  :ensure t
  :after (:all projectile helm)
  :config
  (helm-projectile-on))

(use-package jinja2-mode
  :ensure t
  :mode (("\\.j2" . jinja2-mode)
         ("\\.jinja" . jinja2-mode)))

(use-package linum-relative
  :ensure t
  :bind ("C-c t l" . linum-relative-toggle))

(use-package lsp-mode
  :ensure t
  :hook (program-mode . lsp)
  :init (setq-default lsp-auto-configure t
                      lsp-enable-imenu t
                      lsp-keymap-prefix "C-;"
                      lsp-diagnostic-package :none
                      lsp-enable-completion-at-point t
                      lsp-before-save-edits t
                      lsp-enable-imenu t
                      lsp-imenu-show-container-name t
                      lsp-enable-indentation t
                      lsp-enable-folding t
                      lsp-semantic-highlighting :immediate)
  :commands (lsp))

(use-package lsp-python-ms
  :demand t
  :ensure t
  :hook (python-mode . lsp))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package markdown-changelog
  :ensure t)

(use-package nginx-mode
  :ensure t)

(use-package org-journal
  :ensure t)

(use-package org-trello
  :ensure t
  :mode ("\\.trello" . org-mode))

(use-package php-mode
  :ensure t)

(use-package poetry
  :ensure t)

(use-package projectile
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (("M-P" . projectile-find-file)
         :map projectile-command-map
         ("p" . projectile-switch-project))
  :init (setq projectile-completion-system 'helm)
  :hook (after-init . projectile-mode))

(use-package py-isort
  :ensure t)

(use-package python-docstring
  :ensure t
  :hook (python-mode . python-docstring-mode))

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1))

(use-package salt-mode
  :ensure t
  :mode "\\.sls")

(use-package sphinx-doc
  :ensure t
  :hook (python-mode . sphinx-doc-mode))

(use-package undohist
  :ensure t
  :hook (after-init . undohist-initialize))

(use-package web-mode
  :ensure t
  :mode ("\\.html" . web-mode))

(use-package which-key
  :ensure t
  :init
  (which-key-setup-minibuffer)
  (setq which-key-popup-type 'minibuffer)
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
 '(custom-safe-themes
   (quote
    ("a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "e269026ce4bbd5b236e1c2e27b0ca1b37f3d8a97f8a5a66c4da0c647826a6664" "e297f54d0dc0575a9271bb0b64dad2c05cff50b510a518f5144925f627bb5832" "0820d191ae80dcadc1802b3499f84c07a09803f2cb90b343678bdb03d225b26b" "3ff96689086ebc06f5f813a804f7114195b7c703ed2f19b51e10026723711e33" "e56ee322c8907feab796a1fb808ceadaab5caba5494a50ee83a13091d5b1a10c" "15835b9d167f29341a0ef018ee05a888621a192588ce31b2b2e9a677252c014c" "e97dbbb2b1c42b8588e16523824bc0cb3a21b91eefd6502879cf5baa1fa32e10" "49de25b465bc3c2498bcd4c1575fa0090bd56fc79cdb49b919b49eaea17ee1dd" "870a63a25a2756074e53e5ee28f3f890332ddc21f9e87d583c5387285e882099" "d3df47c843c22d8f0177fe3385ae95583dc8811bd6968240f7da42fd9aa51b0b" "667e296942c561382fe0a8584c26be0fe7a80416270c3beede8c6d69f2f77ccc" "97f9438943105a17eeca9f1a1c4c946765e364957749e83047d6ee337b5c0a73" "7e6c8ce78135d1f5c89182fd0e4317bb172786be5b1120b75ecceb334482a30b" "614f8478963ec8caac8809931c9d00f670e4519388c02f71d9d27b66d5741a7f" "3164a65923ef23e0f3dff9f9607b4da1e07ef1c3888d0f6878feef6c28357732" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "be50a45342f47158a8d34f54ffecc63f55dbdf66ad672c171c48e9dac56fff52" "c57f41a7e5ac48d462fdd89a5a01676f7643653488786a1e58cfb8b4889b4559" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "1177fe4645eb8db34ee151ce45518e47cc4595c3e72c55dc07df03ab353ad132" "e80a0a5e1b304eb92c58d0398464cd30ccbc3622425b6ff01eea80e44ea5130e" "f2f4cfa0945a2f63b9b30e756dc086a1365e87763de6ddd87512f2812c77a8d6" "7c82c94f0d0a4bbdb7cda2d0587f4ecbbe543f78f2599e671af4d74cea1ae698" "96b023d1a6e796bab61b472f4379656bcac67b3af4e565d9fb1b6b7989356610" default)))
 '(desktop-minor-mode-table
   (quote
    ((auto-fill-function auto-fill-mode)
     (defining-kbd-macro nil)
     (isearch-mode nil)
     (vc-mode nil)
     (vc-dired-mode nil)
     (erc-track-minor-mode nil)
     (savehist-mode nil)
     (eproject-mode eproject-maybe-turn-on))))
 '(elpy-mode-hook (quote (subword-mode hl-line-mode)))
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-sane-defaults)))
 '(elpy-rpc-backend "jedi")
 '(elpy-test-runner (quote elpy-test-pytest-runner))
 '(hl-sexp-background-color "#1c1f26")
 '(magit-commit-arguments (quote ("-S")))
 '(magit-log-section-arguments (quote ("-n256" "--decorate")))
 '(mmm-global-mode nil nil (mmm-mode))
 '(org-trello-current-prefix-keybinding "C-c o" nil (org-trello))
 '(package-selected-packages
   (quote
    (xterm-color xonsh-mode xml-rpc which-key wgrep-ag web-mode use-package undohist tblui systemd sphinx-doc salt-mode rust-mode reformatter python-docstring pytest pyenv-mode-auto py-isort poetry plantuml-mode php-mode persist pandoc pallet ox-qmd ox-pandoc org-trello org-journal nginx-mode metaweblog markdown-changelog lush-theme lsp-ui lsp-python-ms linum-relative kubernetes js2-refactor jinja2-mode jedi inf-ruby hydra htmlize helm-pydoc helm-projectile helm-make helm-lsp helm-flycheck helm-company helm-ag-r helm-ag haml-mode github-search git-link git-gutter-fringe+ git forge flycheck-pyflakes flycheck-mypy flx fill-column-indicator erlang elscreen elpy dockerfile-mode docker discover direx dash-docs company-web company-terraform company-quickhelp company-nginx company-go auto-highlight-symbol anaconda-mode all-the-icons alert ag)))
 '(scroll-bar-mode nil)
 '(scroll-conservatively 10000)
 '(scroll-step 1)
 '(shell-command-with-editor-mode t)
 '(tramp-syntax (quote default) nil (tramp))
 '(url-handler-mode t)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(set-cursor-color "white")

(provide 'init)
;;; init.el ends here
