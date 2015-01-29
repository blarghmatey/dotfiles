;;; package --- Summary
;;; Commentary:
;;; This file contains configuration options for various packages and editor options

;;; Code:
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/usr/local/sbin"))
(setq exec-path (append exec-path '("/usr/local/bin" "/usr/local/sbin")))

(setq ido-enable-flex-matching t)

(setq whitespace-line-column 150)
(setq whitespace-style
      '(face trailing empty lines-tail tab-mark))

(setq fci-rule-column 80)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.sls\\'" . yaml-mode))

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

(setq jedi:complete-on-dot t)
(setq jedi:setup-keys t)

(setq flycheck-flake8-maximum-complexity 10)

(setq tern-command (cons (executable-find "tern") '()))
;; (setq tern-ac-dot-complete t)

(setq frame-title-format "emacs -- %f -- %m")

(global-linum-mode 1) ; Show line numbers
(column-number-mode 1) ; Show cursor column position

(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent 'complete)
(setq-default major-mode 'text-mode)
(setq indent-line-function 'indent-for-tab-command)

(require 'nose)

;; (require 'workgroups2)
;; (setq wg-session-file "~/.emacs.d/.emacs_workgroups")
;; (setq wg-session-load-on-start 1)

(setq company-idle-delay 5)
(setq company-auto-complete nil)
(setq company-minimum-prefix-length 0)

(set-face-attribute 'default nil :font "Source Code Pro-9")

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
(define-key global-map (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-c e") 'eval-and-replace)
(global-set-key (kbd "C-x p") 'other-window-backward)

(global-set-key (kbd "C-c t") 'visit-ansi-term)
(global-set-key (kbd "C-c P") 'run-python)
(global-set-key (kbd "C-c g") 'google)

(global-set-key (kbd "C-c D")  'delete-file-and-buffer)

(global-set-key (kbd "C-x :") 'goto-line)

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

;; Show matching parens
(show-paren-mode)

;; Allow direct edting of permission flags in wdired
(defvar wdired-allow-to-change-permissions t)

;; Don't ask to delete excess versions of files
(defvar trim-versions-without-asking t)

(require 'ido)
(ido-mode t)

(setq evil-toggle-key "C-`")

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
(setq mmm-global-mode t)
;;; end MMM-Mode

;; Jedi Mode
(defvar jedi-config:with-virtualenv nil
"Set to non-nil to point to a particular virtualenv.")
;; Variables to help find the project root
(defvar jedi-config:vcs-root-sentinel ".git")
(defvar jedi-config:python-module-sentinel "__init__.py")

(provide 'configurations)
;;; configurations.el ends here
