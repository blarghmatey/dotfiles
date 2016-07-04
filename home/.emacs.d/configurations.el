;;; package --- Summary
;;; Commentary:
;;; This file contains configuration options for various packages and editor options

;;; Code:
;; (setq mac-option-key-is-meta nil)
;; (setq mac-command-key-is-meta t)
;; (setq mac-command-modifier 'meta)
;; (setq mac-option-modifier nil)
;; (setq debug-on-error t)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/usr/local/sbin"))
(setq exec-path (append exec-path '("/usr/local/bin" "/usr/local/sbin")))

(require 'helm)
(helm-autoresize-mode 1)
(setq helm-mode-fuzzy-match t)
(setq helm-completion-in-region-fuzzy-match t)
(setq helm-M-x-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)

(setq helm-dash-browser-func 'eww)

(setq ido-enable-flex-matching t)

(setq whitespace-line-column 150)
(setq whitespace-style
      '(face trailing empty lines-tail tab-mark))

(setq fci-rule-column 80)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.html" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jinja" . jinja2-mode))
(add-to-list 'auto-mode-alist '("\\.sls" . yaml-mode))

(setq speedbar-indentation-width 2)
(setq speedbar-show-unknown-files t)
(setq speedbar-smart-directory-expand-flag nil)
(setq speedbar-use-images nil)
(setq sr-speedbar-right-side nil)
(setq sr-speedbar-skip-other-window-p t)

;; (setq ac-auto-show-menu t)
;; (setq ac-expand-on-auto-complete t)
;; (setq ac-show-menu-immediately-on-auto-complete t)
;; (setq ac-fuzzy-enable t)

;; (setq jedi:complete-on-dot t)
;; (setq jedi:setup-keys t)
(require 'undohist)
(undohist-initialize)

(require 'flycheck)
(setq flycheck-flake8-maximum-complexity 15)
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))
(flycheck-add-mode 'javascript-eslint 'js2-mode)

(setq tern-command (cons (executable-find "tern") '()))
;; (setq tern-ac-dot-complete t)

(setq frame-title-format "emacs -- %f -- %m")

(global-linum-mode 1) ; Show line numbers
(column-number-mode 1) ; Show cursor column position

(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
(setq split-width-threshold 80)
(setq split-height-threshold 80)
(setq apropos-sort-by-scores t)

(setq-default tab-width 4)
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent 'complete)
(setq-default major-mode 'text-mode)
(setq indent-line-function 'indent-for-tab-command)

(setq magit-last-seen-setup-instructions "1.4.0")

(setq create-lockfiles nil)
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; (require 'workgroups2)
;; (setq wg-session-file "~/.emacs.d/.emacs_workgroups")
;; (setq wg-session-load-on-start 1)

(setq company-idle-delay 1)
(setq company-auto-complete nil)
(setq company-minimum-prefix-length 0)

;; (set-face-attribute 'default nil :font "Source Code Pro-12")
;; Had to use default-frame-alist to fix crash when starting in daemon mode
(setq default-frame-alist '((font . "Source Code Pro-10") (load-theme 'material)))
;; (load-theme 'material)
;; (defvar newline-and-indent t
;;   "Modify the behavior of the open-*-line functions to cause them to autoindent.")


;; (scroll-bar-mode -1)
(tool-bar-mode -1)

; To automatically enter closing pair when opening pair is entered
(electric-pair-mode +1)

(electric-indent-mode 1)

;; Ruby
;; Don't indent parameters inside parens more than normal
(defvar ruby-deep-indent-paren nil)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C-_") 'text-scale-decrease)
(global-set-key (kbd "C-c f p") (lambda () (interactive) (set-face-attribute 'default nil :font "Source Code Pro-14")))
(global-set-key (kbd "C-c f m") (lambda () (interactive) (set-face-attribute 'default nil :font "Source Code Pro-10")))
(global-set-key (kbd "C-c f t") (lambda () (interactive) (set-face-attribute 'default nil :font "Source Code Pro-5")))

(global-set-key (kbd "C-c s l") 'linum-relative-toggle)

(global-set-key (kbd "C-c e") 'eval-and-replace)
(global-set-key (kbd "C-x p") 'other-window-backward)

(global-set-key (kbd "C-c t") 'visit-ansi-term)
(global-set-key (kbd "C-c P") 'run-python)
(global-set-key (kbd "C-c g") 'google)

(global-set-key (kbd "C-c D")  'delete-file-and-buffer)

(global-set-key (kbd "C-:") 'goto-line)

(global-set-key (kbd "C-M-z") 'indent-defun)
; (global-set-key (kbd "C-c o") 'open-with)
;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(global-set-key (kbd "M-X") 'smex)

(global-set-key (kbd "C-o") 'open-next-line)
(global-set-key (kbd "M-o") 'open-previous-line)

(global-set-key (kbd "M-s t") 'speedbar)
(global-set-key (kbd "M-s f") 'speedbar-get-focus)

(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "C-c C-.") 'anaconda-mode-goto)
(global-set-key (kbd "C-c C-,") 'anaconda-nav-pop-marker)
(global-set-key (kbd "C-c /") 'anaconda-mode-usages)

(global-set-key (kbd "C-c l") `windmove-right)
(global-set-key (kbd "C-c k") `windmove-up)
(global-set-key (kbd "C-c j") `windmove-down)
(global-set-key (kbd "C-c h") `windmove-left)

(global-set-key (kbd "C->") 'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "C-<") 'indent-rigidly-left-to-tab-stop)

(global-set-key [remap dabbrev-expand] 'hippie-expand)

(global-set-key (kbd "C-c L") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)

(global-set-key (kbd "C-c C-\"") 'insert-pair)
(global-set-key (kbd "C-c C-'") 'insert-pair)
(global-set-key (kbd "C-c C-(") 'insert-pair)
(global-set-key (kbd "C-c C-{") 'insert-pair)
(global-set-key (kbd "C-c C-[") 'insert-pair)

(global-set-key (kbd "C-M-p") 'projectile-find-file)

(global-set-key (kbd "M-%") 'query-replace-regexp)

(global-set-key [f1] 'toggle-fold-to-signatures)

(setq org-log-done t)
;; Show matching parens
(show-paren-mode)

;; Allow direct edting of permission flags in wdired
(defvar wdired-allow-to-change-permissions t)

;; Don't ask to delete excess versions of files
(defvar trim-versions-without-asking t)

(require 'ido)
(ido-mode t)

;; (setq evil-toggle-key "C-`")
;; (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
;; (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
;; (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
;; (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
;; (setq evil-default-cursor "white")

;;
;; Unscroll support
;;

(put 'scroll-up 'unscrollable t)
(put 'scroll-down 'unscrollable t)
(put 'scroll-left 'unscrollable t)
(put 'scroll-right 'unscrollable t)

(defvar unscroll-point (make-marker)
  "Cursor position for next call to unscroll")
(defvar unscroll-window-start (make-marker)
  "Window start position for next call to unscroll")
(defvar unscroll-hscroll nil
  "Hscroll position for next call to unscroll")

(when window-system
  (blink-cursor-mode 0))
(set-cursor-color "#ffffff")

(defvar emacs-configuration-directory
    "~/.emacs.d/"
    "The directory where the emacs configuration files are stored.")
(defvar elscreen-tab-configuration-store-filename
    (concat emacs-configuration-directory ".elscreen")
    "The file where the elscreen tab configuration is stored.")


(defvar hexcolor-keywords
  '(("#[abcdef[:digit:]]+"
     (0 (put-text-property
         (match-beginning 0)
         (match-end 0)
         'face (list :background
                     (match-string-no-properties 0)))))))
;;;
;;; MMM-Mode definitions
;;;

(setq mmm-submode-decoration-level 1)

(mmm-add-classes '((saltstack-mode
                  :submode jinja2-mode
                  :face mmm-declaration-submode-face
                  :front "{[{%].+?"
                  :front-offset -3
                  :back "[}%]}"
                  :back-offset 2)))


;; (mmm-add-group 'saltstack-mode
;;                '((saltstack-mode-jinja
;;                   :submode jinja2-mode
;;                   :face mmm-declaration-submode-face
;;                   :front "{[{%].+?"
;;                   :front-offset -2
;;                   :back "[}%]}"
;;                   :back-offset 2)
;;                (saltstack-mode-python
;;                   :submode python-mode
;;                   :face mmm-declaration-submode-face
;;                   :front "salt[[].+?"
;;                   :front-offset -5
;;                   :back ")"
;;                   :back-offset 1)))

(mmm-add-mode-ext-class 'yaml-mode "\\.sls\\'" 'saltstack-mode)
(setq mmm-global-mode nil)
;;; end MMM-Mode

;; Jedi Mode
(defvar jedi-config:with-virtualenv nil
"Set to non-nil to point to a particular virtualenv.")
;; Variables to help find the project root
(defvar jedi-config:vcs-root-sentinel ".git")
(defvar jedi-config:python-module-sentinel "__init__.py")

(provide 'configurations)
;;; configurations.el ends here
