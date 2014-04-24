;;; package --- Summary
;;; Commentary:
;;; This file pulls in the functions and variables from functions.el and
;;; configurations.el as well as providing initialization routines

;;; Code:
(load-file "~/.emacs.d/functions.el")
(load-file "~/.emacs.d/configurations.el")
;; (setq load-path (append load-path '("~/.emacs.d/"))
;; (require 'functions)
;; (require 'configurations)

(add-hook 'kill-emacs-hook 'save-package-list)
(load-file "~/Dropbox/.emacs-packages-installed.el")
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(projectile-global-mode)
(global-whitespace-mode)
(global-auto-revert-mode t)
(elscreen-start)
(powerline-center-theme)
(autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on `ctags-auto-update-mode'." t)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'program-mode-hook 'default-minor-modes)
(add-hook 'web-mode 'default-minor-modes)
(add-hook 'python-mode-hook 'default-minor-modes)
(add-hook 'python-mode-hook (lambda () (interactive) (column-marker-1 80)))
(add-hook 'python-mode-hook 'pyvenv-mode)
(add-hook 'ruby-mode-hook 'default-minor-modes)
(add-hook 'emacs-lisp-mode-hook 'default-minor-modes)
(add-hook 'js-mode-hook 'default-minor-modes)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(add-hook 'js2-mode-hook 'default-minor-modes)
(add-hook 'js2-mode-hook 'tern-mode)

;; (add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook 'global-auto-complete-mode) ;; Trying out auto-complete instead of company
(add-hook 'after-init-hook 'global-flycheck-mode)

(when (not package-archive-contents)
  (package-refresh-contents))

;; enable subword (CamelCase-aware) just in ruby-mode
(add-hook 'ruby-mode-hook 'subword-mode)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("96b023d1a6e796bab61b472f4379656bcac67b3af4e565d9fb1b6b7989356610" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
