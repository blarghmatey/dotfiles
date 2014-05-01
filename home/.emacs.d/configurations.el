;;; package --- Summary
;;; Commentary:
;;; This file contains configuration options for various packages and editor options

;;; Code:
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;;; Package manager settings
(require 'package)
(add-to-list 'package-archives
   '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
   '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(setq ido-enable-flex-matching t)

(setq whitespace-line-column 150)
(setq whitespace-style
      '(face trailing empty lines-tail tab-mark))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(setq speedbar-indentation-width 2)
(setq speedbar-show-unknown-files t)
(setq speedbar-smart-directory-expand-flag nil)
(setq speedbar-use-images nil)
(setq sr-speedbar-right-side nil)
(setq sr-speedbar-skip-other-window-p t)

(setq ac-auto-show-menu t)
(setq ac-expand-on-auto-complete t)
(setq ac-show-menu-immediately-on-auto-complete t)
(setq ac-fuzzy-enable t)

(setq jedi:complete-on-dot t)
(setq jedi:setup-keys t)

(global-linum-mode 1) ; Show line numbers
(column-number-mode 1) ; Show cursor column position
(desktop-save-mode 1) ; Offer to save/restore open buffers

(setq-default indent-tabs-mode nil)

(setq company-idle-delay t)

(set-face-attribute 'default nil :font "Source Code Pro-9")

(defvar newline-and-indent t
  "Modify the behavior of the open-*-line functions to cause them to autoindent.")


(scroll-bar-mode -1)
(tool-bar-mode -1)
(electric-indent-mode +1)

; To automatically enter closing pair when opening pair is entered
(electric-pair-mode +1)

;; Ruby
;; Don't indent parameters inside parens more than normal
(defvar ruby-deep-indent-paren nil)

(setq tab-width 4)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-c e") 'eval-and-replace)
(global-set-key (kbd "C-x p") 'other-window-backward)

(global-set-key (kbd "C-c t") 'visit-ansi-term)
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

(global-set-key (kbd "M-s t") 'sr-speedbar-toggle)
(global-set-key (kbd "M-s f") 'sr-speedbar-select-window)

(global-set-key (kbd "C-x :") 'goto-line)
;; Show matching parens
(show-paren-mode)

;; Allow direct edting of permission flags in wdired
(defvar wdired-allow-to-change-permissions t)

;; Don't ask to delete excess versions of files
(defvar trim-versions-without-asking t)

(require 'ido)
(ido-mode t)

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

(provide 'configurations)
;;; configurations.el ends here
