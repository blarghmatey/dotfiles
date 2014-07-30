;;; package --- Summary
;;; Commentary:
;;; This file pulls in the functions and variables from functions.el and
;;; configurations.el as well as providing initialization routines

;;; Code:

(load-file "~/.emacs.d/functions.el")
(add-hook 'after-init-hook 'load-packages)

(load-file "~/.emacs.d/configurations.el")
(setq load-path (append load-path '("~/.emacs.d/plugins")))
;; (require 'functions)
;; (require 'configurations)
(when (not package-archive-contents)
  (package-refresh-contents))

(load-file "~/Dropbox/.emacs-packages-installed.el")
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-hook 'kill-emacs-hook 'save-package-list)

(projectile-global-mode)
(global-whitespace-mode)
(global-auto-revert-mode t)
(elscreen-start)
(powerline-center-theme)
(autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on `ctags-auto-update-mode'." t)


(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'program-mode-hook 'default-minor-modes)
(add-hook 'web-mode 'default-minor-modes)
(add-hook 'python-mode-hook 'pyvenv-mode)
(add-hook 'python-mode-hook 'jedi-setup-venv)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'flycheck-python-setup)
(add-hook 'python-mode-hook 'fci-mode)
(add-hook 'python-mode-hook 'default-minor-modes)
(add-hook 'ruby-mode-hook 'default-minor-modes)
(add-hook 'emacs-lisp-mode-hook 'default-minor-modes)
(add-hook 'js2-mode-hook 'tern-mode)
(add-hook 'js2-mode-hook 'tern-ac-setup)
(add-hook 'js2-mode-hook 'default-minor-modes)
;; enable subword (CamelCase-aware) just in ruby-mode
(add-hook 'ruby-mode-hook 'subword-mode)

;; (add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook 'global-auto-complete-mode) ;; Trying out auto-complete instead of company
(add-hook 'after-init-hook 'global-flycheck-mode)

;; (add-hook 'after-init-hook (lambda () (load-theme 'dakrone)))
(color-theme-monokai)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "1177fe4645eb8db34ee151ce45518e47cc4595c3e72c55dc07df03ab353ad132" "e80a0a5e1b304eb92c58d0398464cd30ccbc3622425b6ff01eea80e44ea5130e" "f2f4cfa0945a2f63b9b30e756dc086a1365e87763de6ddd87512f2812c77a8d6" "7c82c94f0d0a4bbdb7cda2d0587f4ecbbe543f78f2599e671af4d74cea1ae698" "96b023d1a6e796bab61b472f4379656bcac67b3af4e565d9fb1b6b7989356610" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
