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

(global-whitespace-mode)
(global-auto-revert-mode t)
(elscreen-start)
(powerline-center-theme)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'program-mode-hook 'default-minor-modes)
(add-hook 'web-mode 'default-minor-modes)
(add-hook 'python-mode-hook 'default-minor-modes)
(add-hook 'ruby-mode-hook 'default-minor-modes)
(add-hook 'emacs-lisp-mode-hook 'default-minor-modes)
(add-hook 'js-mode-hook 'default-minor-modes)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(add-hook 'js2-mode-hook 'default-minor-modes)
(add-hook 'js2-mode-hook 'tern-mode)

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook 'global-flycheck-mode)

(when (not package-archive-contents)
  (package-refresh-contents))

;; enable subword (CamelCase-aware) just in ruby-mode
(add-hook 'ruby-mode-hook 'subword-mode)

(provide 'init)
;;; init.el ends here
