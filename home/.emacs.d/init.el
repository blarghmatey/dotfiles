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

(projectile-global-mode)
(global-whitespace-mode)
(global-auto-revert-mode t)
(require 'git-gutter-fringe+)
(global-git-gutter+-mode)
(global-evil-leader-mode)
(evil-leader/set-leader "'")
(evil-mode 1)
(elscreen-start)
;; (elscreen-restore)
(run-with-timer 300 (* 5 60) 'elscreen-store)
;; (powerline-center-theme)
(autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on `ctags-auto-update-mode'." t)

(require 'jedi)
;; (add-to-list 'ac-sources 'ac-source-jedi-direct)

;; CSS color values colored by themselves

;; http://news.ycombinator.com/item?id=873541

;; (add-hook 'css-mode-hook 'hexcolor-add-to-font-lock)

;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)
;; (autoload 'pymacs-autoload "pymacs")
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'program-mode-hook 'default-minor-modes)
(add-hook 'program-mode-hook (lambda () (push 'company-capf completion-at-point-functions)))
(add-hook 'web-mode 'default-minor-modes)
(add-hook 'python-mode-hook 'pyvenv-mode)
(add-hook 'python-mode-hook 'setup-venv)
(add-hook 'python-mode-hook 'flycheck-python-setup)
(add-hook 'python-mode-hook 'fci-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)
(add-hook 'python-mode-hook 'nose-mode)
;; (add-hook 'python-mode-hook 'default-minor-modes)
;; (add-hook 'python-mode-hook 'pyvenv-mode)
;; (add-hook 'python-mode-hook 'setup-venv)
;; (add-hook 'python-mode-hook (lambda () (anaconda-mode)))
;; (add-hook 'python-mode-hook 'company-anaconda)
;; (add-hook 'python-mode-hook 'jedi-mode)
;; (add-hook 'python-mode-hook 'flycheck-python-setup)
;; (add-hook 'python-mode-hook 'fci-mode)
;; (add-hook 'python-mode-hook 'nose-mode)
(add-hook 'ruby-mode-hook 'default-minor-modes)
(add-hook 'emacs-lisp-mode-hook 'default-minor-modes)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
(add-hook 'window-configuration-change-hook
          (lambda ()
            (when (equal major-mode 'python-mode)
              (setup-venv)
              (anaconda-mode-virtualenv-has-been-changed)
              ;; (jedi:setup)
              )))
(eval-after-load 'tern
  '(progn
     ;; (require 'tern-auto-complete)
     ;; (tern-ac-setup)
     (company-tern)
     ))
(add-hook 'js2-mode-hook 'default-minor-modes)
(add-hook 'yaml-mode-hook 'default-minor-modes)
(add-hook 'yaml-mode-hook (lambda () (setq-local electric-indent-mode -1)))
;; enable subword (CamelCase-aware) just in ruby-mode
(add-hook 'ruby-mode-hook 'subword-mode)

(add-hook 'after-init-hook 'global-company-mode)
;; (add-hook 'after-init-hook 'global-auto-complete-mode) ;; Trying out auto-complete instead of company
(add-hook 'after-init-hook 'global-flycheck-mode)

(lush-theme)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "1177fe4645eb8db34ee151ce45518e47cc4595c3e72c55dc07df03ab353ad132" "e80a0a5e1b304eb92c58d0398464cd30ccbc3622425b6ff01eea80e44ea5130e" "f2f4cfa0945a2f63b9b30e756dc086a1365e87763de6ddd87512f2812c77a8d6" "7c82c94f0d0a4bbdb7cda2d0587f4ecbbe543f78f2599e671af4d74cea1ae698" "96b023d1a6e796bab61b472f4379656bcac67b3af4e565d9fb1b6b7989356610" default)))
 '(mmm-global-mode nil nil (mmm-mode))
 '(safe-local-variable-values (quote ((fci-rule-column . 121)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
