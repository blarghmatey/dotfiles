;;; package --- Summary
;; Commentary:
;;; This file pulls in the functions and variables from functions.el and
;;; configurations.el as well as providing initialization routines

;;; Code:

;;; Package manager settings
(require 'package)
(add-to-list 'package-archives
   '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
   '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(setq load-path (append load-path '("~/.emacs.d/plugins")))
(load-file "~/.emacs.d/functions.el")
(add-hook 'after-init-hook 'load-packages)

(load-file "~/.emacs.d/configurations.el")

;; (require 'functions)
;; (require 'configurations)
(when (not package-archive-contents)
  (package-refresh-contents))

(load-file "~/Dropbox/.emacs-packages-installed.el")
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-hook 'kill-emacs-hook 'save-package-list)
(add-hook 'kill-emacs-hook 'elscreen-store)
;; (add-hook 'kill-emacs-hook 'wg-save-session)

;; (workgroups-mode 1)

(helm-mode)
(projectile-global-mode)
(global-whitespace-mode)
(global-auto-revert-mode t)
(require 'git-gutter-fringe+)
(global-git-gutter+-mode)
;; (global-evil-leader-mode)
;; (evil-leader/set-leader "'")
;; (evil-mode 1)
;; (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
;; (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
;; (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
;; (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(elscreen-start)
(run-with-timer 300 (* 5 60) 'elscreen-store)
(autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on `ctags-auto-update-mode'." t)

;; (require 'jedi)
;; (add-to-list 'ac-sources 'ac-source-jedi-direct)

;; CSS color values colored by themselves

;; http://news.ycombinator.com/item?id=873541

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'program-mode-hook 'default-minor-modes)
;; (add-hook 'program-mode-hook (lambda () (push 'company-capf completion-at-point-functions)))
(add-hook 'web-mode 'default-minor-modes)
(add-hook 'python-mode-hook 'pyvenv-mode)
(add-hook 'python-mode-hook 'setup-venv)
(add-hook 'python-mode-hook 'flycheck-python-setup)
(add-hook 'python-mode-hook 'fci-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)
(add-hook 'python-mode-hook 'nose-mode)
(add-hook 'python-mode-hook 'sphinx-doc-mode)
(add-hook 'ruby-mode-hook 'default-minor-modes)
(add-hook 'emacs-lisp-mode-hook 'default-minor-modes)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
(add-hook 'window-configuration-change-hook
          (lambda ()
            (when (equal major-mode 'python-mode)
              (setup-venv)
              )))
;; (eval-after-load 'tern
;;   '(progn
;;      ;; (require 'tern-auto-complete)
;;      ;; (tern-ac-setup)
;;      (company-tern)
;;      ))
(add-hook 'js2-mode-hook 'default-minor-modes)
(add-hook 'yaml-mode-hook 'default-minor-modes)
(add-hook 'yaml-mode-hook (lambda () (setq-local electric-indent-mode -1)))
;; enable subword (CamelCase-aware) just in ruby-mode
(add-hook 'ruby-mode-hook 'subword-mode)
(add-hook 'markdown-mode-hook (lambda () (setq-local whitespace-style
      '(face trailing empty tab-mark))))

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook 'global-flycheck-mode)
;; (add-hook 'neotree-mode-hook
;;             (lambda ()
;;               (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
;;               (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
;;               (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
;;               (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

(lush-theme)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3164a65923ef23e0f3dff9f9607b4da1e07ef1c3888d0f6878feef6c28357732" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "be50a45342f47158a8d34f54ffecc63f55dbdf66ad672c171c48e9dac56fff52" "c57f41a7e5ac48d462fdd89a5a01676f7643653488786a1e58cfb8b4889b4559" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "1177fe4645eb8db34ee151ce45518e47cc4595c3e72c55dc07df03ab353ad132" "e80a0a5e1b304eb92c58d0398464cd30ccbc3622425b6ff01eea80e44ea5130e" "f2f4cfa0945a2f63b9b30e756dc086a1365e87763de6ddd87512f2812c77a8d6" "7c82c94f0d0a4bbdb7cda2d0587f4ecbbe543f78f2599e671af4d74cea1ae698" "96b023d1a6e796bab61b472f4379656bcac67b3af4e565d9fb1b6b7989356610" default)))
 '(mmm-global-mode nil nil (mmm-mode))
 '(org-mobile-directory "~/Dropbox/org")
 '(package-selected-packages
   (quote
    (org-mobile-sync feature-mode twittering-mode magit-gh-pulls zeal-at-point yaml-mode xlicense xcscope windata wgrep-ag web-mode wakatime-mode virtualenvwrapper vagrant tree-mode tj-mode tern-auto-complete systemd sx swiper-helm stekene-theme sr-speedbar sql-indent spinner sphinx-doc spacemacs-theme soothe-theme smex smeargle smartparens smart-mode-line seq scss-mode sass-mode rvm rust-mode rinari rainbow-mode rainbow-delimiters python-docstring python-django pytest pymacs py-yapf py-isort py-import-check py-autopep8 puppetfile-mode puppet-mode projectile-rails prodigy powerline polymode pip-requirements php-mode persp-projectile pep8 pallet org-jira nose-mode nodejs-repl nim-mode nginx-mode neotree multi-web-mode multi-term multi mmm-mode mkdown material-theme magit-filenotify lush-theme logstash-conf lice lentic ldap-mode late-night-theme kivy-mode jsx-mode json-mode js3-mode jinja2-mode jedi-direx ivy iedit idomenu history heroku helm-pydoc helm-projectile helm-proc helm-git helm-flycheck helm-dash helm-company helm-cmd-t helm-aws helm-ag git-link git-gutter-fringe+ git gh-md fuzzy format-sql fold-this flycheck-rust flycheck-color-mode-line flx-isearch floobits fill-column-indicator ess eproject elscreen-persist elpy el-get ein dpaste docean distinguished-theme discover-my-major discover-js2-refactor dash-at-point dart-mode dark-souls dakrone-theme cython-mode ctags-update ctags csv-mode css-eldoc company-ycm company-web company-tern company-quickhelp company-jedi company-inf-ruby company-anaconda commander command-t column-marker color-theme-twilight color-theme-solarized color-theme-sanityinc-solarized color-theme-monokai color-theme-approximate coffee-mode cloc chruby bundler browse-at-remote auto-complete-rst auto-complete-exuberant-ctags assemblage-theme ansible anaphora ahg ag ac-js2 ac-inf-ruby ac-capf)))
 '(safe-local-variable-values
   (quote
    ((flycheck-jshintrc . ".jshintrc")
     (flycheck-flake8rc . "setup.cfg")
     (pyvenv-workon "hapyak")
     (pyvenv-workon "hapyak-reporting")
     (fci-rule-column . 121)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(set-cursor-color "white")

(provide 'init)
;;; init.el ends here
