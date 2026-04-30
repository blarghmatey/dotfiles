;;; package --- Summary
;;; Commentary:
;;; This file contains configuration options for various packages and editor options

;;; Code:
;; (setq mac-option-key-is-meta nil)
;; (setq mac-command-key-is-meta t)
;; (setq mac-command-modifier 'meta)
;; (setq mac-option-modifier nil)
;; (setq debug-on-error t)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/usr/local/sbin:/home/tmacey/.local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin" "/usr/local/sbin" "/home/tmacey/.local/bin")))

(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo"  "~/.netrc"))

(require 'delight)
(delight '((global-whitespace-mode nil "whitespace")
           (subword-mode nil "subword")
           (flyspell-mode nil "flyspell")))

(setq ring-bell-function 'ignore)
(setq whitespace-line-column 150
      whitespace-style
      '(face trailing empty lines-tail tab-mark))

(add-hook 'yaml-ts-mode-hook
          (lambda ()
            (setq-local indent-line-function #'indent-rigidly)))

;; Better support for using emacsclient
(setq kill-emacs-query-functions
      (cons (lambda () (yes-or-no-p "Really kill Emacs? "))
            kill-emacs-query-functions))

(add-hook 'server-switch-hook
          (lambda ()
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
            (when server-buffer-clients
              (local-set-key (kbd "C-x k") 'server-edit))))
;; end emacsclient

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("sourcery" "lsp"))
                  :initialization-options `((token . ,(auth-source-pick-first-password :host "sourcery.ai"))
                                            (extension_version . "emacs-lsp")
                                            (editor_version . "emacs"))
                  :activation-fn (lsp-activate-on "python")
                  :server-id 'sourcery
                  :add-on? t))

;;; Commentary:

;; LSP Clients for the Python(pyrefly) Type Checker .

;;; Code:

(require 'lsp-mode)

(defgroup lsp-python-refly nil
  "LSP support for Python(pyrefly)."
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

(lsp-consistency-check lsp-python-refly)

(provide 'lsp-python-refly)

(setq-default fill-column 88)

;; (setq speedbar-indentation-width 2
;;       speedbar-show-unknown-files t
;;       speedbar-smart-directory-expand-flag nil
;;       speedbar-use-images nil
;;       sr-speedbar-right-side nil
;;       sr-speednbar-skip-other-window-p t)

(setq frame-title-format "emacs -- %f -- %m")

(global-display-line-numbers-mode 1)
(column-number-mode 1) ; Show cursor column position
(show-paren-mode) ; Show matching parens

(setq warning-minimum-level :error)

(set-scroll-bar-mode nil)
(setq scroll-step 1
      scroll-conservatively 10000
      auto-window-vscroll nil
      split-width-threshold 70
      split-height-threshold 100
      apropos-sort-by-scores t)
(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'horizontal-scroll-bar-mode nil)

(setq-default indent-tabs-mode nil
              major-mode 'text-mode
              tab-always-indent 'complete
              tab-width 4)
(setq default-tab-width 4
      indent-line-function 'indent-for-tab-command
      indent-tabs-mode nil)

(setq create-lockfiles nil)
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.saves"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Had to use default-frame-alist to fix crash when starting in daemon mode
(setq default-frame-alist '((font . "Hack-12") (load-theme 'lush)))
(set-fontset-font t 'emoji '("Noto Color Emoji" . "iso10646-1") nil 'prepend)

(tool-bar-mode -1)

; To automatically enter closing pair when opening pair is entered
(electric-pair-mode +1)
(electric-indent-mode 1)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C-_") 'text-scale-decrease)
(global-set-key (kbd "C-c f p") (lambda () (interactive) (set-face-attribute 'default nil :font "Hack-16")))
(global-set-key (kbd "C-c f n") (lambda () (interactive) (set-face-attribute 'default nil :font "Hack-12")))
(global-set-key (kbd "C-c f m") (lambda () (interactive) (set-face-attribute 'default nil :font "Hack-14")))
(global-set-key (kbd "C-c f t") (lambda () (interactive) (set-face-attribute 'default nil :font "Hack-10")))
(global-set-key (kbd "C-c f u") (lambda () (interactive) (set-face-attribute 'default nil :font "Hack-8")))
(global-set-key (kbd "C-c f f") (lambda () (interactive) (set-face-attribute 'default nil :font "Hack-7")))

;; Automatically handle theme changes with system light/dark mode
;; from https://emacs.stackexchange.com/a/71164
;; (when (and IS-LINUX ;; this is doom specific
;;            (featurep! :ui dbus)) ;; so is this
;;   ;; I should use a better name than `a`
;;   (defun theme--handle-dbus-event (a setting values)
;;     "Handler for FreeDesktop theme changes."
;;     (when (string= setting "ColorScheme")
;;       (let ((scheme (car values)))
;;         (cond
;;          ((string-match-p "Dark" scheme)
;;           (+theme-dark)) ;; my custom function that sets a dark theme
;;          ((string-match-p "Light" scheme)
;;           (+theme-light)) ;; 1000 internet points to whoever guesses what this does
;;          (t (message "I don't know how to handle scheme: %s" scheme))))))

;;   (require 'dbus)

;;   ;; since this is all FreeDesktop stuff, this *might* work on GNOME without changes
;;   (dbus-register-signal :session
;;                         "org.freedesktop.portal"
;;                         "/org/freedesktop/portal/desktop"
;;                         "org.freedesktop.impl.portal.Settings"
;;                         "SettingChanged"
;;                         #'theme--handle-dbus-event))

;; Alternative implementation from
;; https://www.reddit.com/r/emacs/comments/o49v2w/automatically_switch_emacs_theme_when_changing/
;; (use-package dbus)
;; (use-package modus-themes
;;   :after (dbus)
;;   :config
;;   (defun set-modus-theme-from-gtk ()
;;     "Set modus theme by checking whether GTK theme is dark."
;;     (let ((gtk-theme (downcase
;;                       (call-process-string "gsettings"
;;                                            "get"
;;                                            "org.gnome.desktop.interface"
;;                                            "gtk-theme"))))
;;       (if (or (string-match-p "dark"  gtk-theme)
;;               (string-match-p "black" gtk-theme))
;;           (modus-themes-load-vivendi)
;;         (modus-themes-load-operandi))))

;;   (defun gtk-theme-changed (path _ _)
;;     "DBus handler to detect when the GTK theme has changed."
;;     (when (string-equal path "/org/gnome/desktop/interface/gtk-theme")
;;       (set-modus-theme-from-gtk)))

;;   (dbus-register-signal
;;    :session
;;    "ca.desrt.dconf"
;;    "/ca/desrt/dconf/Writer/user"
;;    "ca.desrt.dconf.Writer"
;;    "Notify"
;;    #'gtk-theme-changed)

;;   (set-modus-theme-from-gtk))

(defun call-process-string (program &rest args)
  "Call process`PROGRAM' with `ARGS' and return the output as string."
  (with-temp-buffer
    (apply #'call-process program nil t nil args)
    (buffer-string)))

;; Use C-c t as a prefix for toggling things
(global-set-key (kbd "C-c i d") 'insert-date)

(global-set-key (kbd "C-c e") 'eval-and-replace)
(global-set-key (kbd "C-x p") 'other-window-backward)

(global-set-key (kbd "C-c C-t") 'visit-ansi-term)
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

(global-set-key (kbd "C-o") 'open-next-line)
(global-set-key (kbd "M-o") 'open-previous-line)

(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "C-c l") `windmove-right)
(global-set-key (kbd "C-c k") `windmove-up)
(global-set-key (kbd "C-c j") `windmove-down)
(global-set-key (kbd "C-c h") `windmove-left)

(global-set-key (kbd "C->") 'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "C-<") 'indent-rigidly-left-to-tab-stop)

(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; (global-set-key (kbd "C-c p p") 'projectile-switch-project)
;; (global-set-key (kbd "M-P") 'projectile-find-file)

(global-set-key (kbd "M-%") 'query-replace-regexp)

(global-set-key [f1] 'toggle-fold-to-signatures)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ORGMODE CONFIGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'org-trello)
;; (setq org-trello-current-prefix-keybinding "C-c o" nil (org-trello))
;; (require 'org-gcal)
(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))

;; Allow direct edting of permission flags in wdired
(defvar wdired-allow-to-change-permissions t)

;; Don't ask to delete excess versions of files
(defvar trim-versions-without-asking t)

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

(defvar hexcolor-keywords
  '(("#[abcdef[:digit:]]+"
     (0 (put-text-property
         (match-beginning 0)
         (match-end 0)
         'face (list :background
                     (match-string-no-properties 0)))))))

(provide 'configurations)
;;; configurations.el ends here
