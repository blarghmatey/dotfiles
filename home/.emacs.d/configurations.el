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

(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo"  "~/.netrc"))

(require 'delight)
(delight '((global-whitespace-mode nil "whitespace")
           (subword-mode nil "subword")
           (flyspell-mode nil "flyspell")))

(setq ring-bell-function 'ignore)
(setq whitespace-line-column 150
      whitespace-style
      '(face trailing empty lines-tail tab-mark))

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
                  :initialization-options '((token . "user_4C2rLVblb6DVCi1QQV_uL4stau_1tjski5VIxux4UAfAePWowD18NSJYw6s")
                                            (extension_version . "emacs-lsp")
                                            (editor_version . "emacs"))
                  :activation-fn (lsp-activate-on "python")
                  :server-id 'sourcery
                  :add-on? t))

(setq-default fill-column 88)
(advice-add 'poetry-venv-toggle :after 'clear-flycheck-auto-disabled-checkers)
(require 'flycheck)

(flycheck-define-checker python-ruff
  "A Python syntax and style checker using the ruff utility.
To override the path to the ruff executable, set
`flycheck-python-ruff-executable'.
See URL `http://pypi.python.org/pypi/ruff'."
  :command ("ruff"
            "--output-format=text"
            (eval (when buffer-file-name
                    (concat "--stdin-filename=" buffer-file-name)))
            "-")
  :standard-input t
  :error-filter (lambda (errors)
                  (let ((errors (flycheck-sanitize-errors errors)))
                    (seq-map #'flycheck-flake8-fix-error-level errors)))
  :error-patterns
  ((warning line-start
            (file-name) ":" line ":" (optional column ":") " "
            (id (one-or-more (any alpha)) (one-or-more digit)) " "
            (message (one-or-more not-newline))
            line-end))
  :modes python-mode)

(add-to-list 'flycheck-checkers 'python-ruff)

(provide 'flycheck-ruff)
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
(require 'org-trello)
;; (setq org-trello-current-prefix-keybinding "C-c o" nil (org-trello))
;; (require 'org-gcal)
(require 'ox-latex)
;; (require 'org-mu4e)
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EMAIL CONFIGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
;; (require 'mu4e)
;; (require 'smtpmail)
;; (require 'mu4e-contrib)
;; (setq send-mail-function 'smtpmail-send-it
;;       message-send-mail-function 'smtpmail-send-it)
;; (setq mu4e-maildir (expand-file-name "~/.mail"))
;; (setq mu4e-use-fancy-chars t)
;; (setq mu4e-view-show-images t)
;; (setq mu4e-show-images t)
;; (setq mu4e-html2text-command 'mu4e-shr2text)
;; (setq shr-use-colors nil)
;; (setq mu4e-get-mail-command "mbsync -a")
;; (setq mu4e-change-filenames-when-moving t)
;; (setq mu4e-completing-read-function 'completing-read)
;; (add-to-list 'mu4e-view-actions
;;              '("ViewInBrowser" . mu4e-action-view-in-browser) t)
;; (add-to-list 'mu4e-view-actions
;;              '("Eww view" . jcs-view-in-eww) t)
;; (add-to-list 'mu4e-headers-fields '(:maildir))
;; (define-key mu4e-view-mode-map "\t" 'shr-next-link)
;; (define-key mu4e-view-mode-map (kbd "<backtab>") 'shr-previous-link)
;; (when (fboundp 'imagemagick-register-types)
;;   (imagemagick-register-types))
;; (setq mu4e-compose-format-flowed t)
;; (setq mu4e-compose-dont-reply-to-self t)
;; (setq mu4e-update-interval 1200)
;; (setq mu4e-view-prefer-html t)
;; (setq mu4e-context-policy nil)
;; (setq message-kill-buffer-on-exit t)
;; (setq org-mu4e-convert-to-html t)
;; (setq mu4e-view-show-addresses t)
;; (setq mu4e-scroll-to-next nil)
;; (setq mu4e-compose-dont-reply-to-self t)
;; (setq mu4e-view-use-gnus t)
;; (setq org-mime-export-options '(:section-numbers nil
;;                                 :with-author nil
;;                                 :with-toc nil))
;; (setq mu4e-user-mail-address-list '("tmacey@boundlessnotions.com"
;;                                     "tmacey@renaissancedev.com"
;;                                     "tmacey@bitlancer.com"
;;                                     "tmacey@mit.edu"
;;                                     "tmacey@podcastinit.com"
;;                                     "tmacey@dataengineeringpodcast.com"
;;                                     "blarghmatey@gmail.com"
;;                                     "tobias.macey@gmail.com"
;;                                     "hosts@podcastinit.com"
;;                                     "hosts@dataengineeringpodcast.com"))
;; (dolist (bookmark
;;          '(("date:7d..now AND NOT maildir:\"/tobiasmacey/Python Ideas\" AND NOT maildir:\"/mitodl/Inbox/Django Errors\"" "Week View" ?W)
;;            ("date:30d..now AND NOT maildir:\"/tobiasmacey/Python Ideas\" AND NOT maildir:\"/mitodl/Inbox/Django Errors\"" "Month View" ?M)
;;            ("date:90d..now AND NOT maildir:\"/tobiasmacey/Python Ideas\" AND NOT maildir:\"/mitodl/Inbox/Django Errors\" flag:unread flag:list" "Unread Newsletters" ?L)
;;            ("flag:flagged" "Flagged Emails" ?f)))
;;   (add-to-list 'mu4e-bookmarks bookmark))
;; (setq mu4e-contexts
;;       `( ,(make-mu4e-context
;;            :name "blarghmatey"
;;            :enter-func (lambda () (define-key mu4e-headers-mode-map (kbd "C-c C-a")
;;                                     (lambda ()
;;                                       (interactive)
;;                                       (let ((mu4e-get-mail-command "mbsync blarghmatey"))
;;                                         (mu4e-update-mail-and-index t)))))
;;            :match-func (lambda (msg)
;;                          (when msg
;;                            (string-prefix-p "/blarghmatey" (mu4e-message-field msg :maildir))))
;;            :vars '((user-mail-address . "blarghmatey@gmail.com")
;;                    (user-full-name . "Tobias Macey")
;;                    (mu4e-trash-folder . "/blarghmatey/[Gmail]/Trash")
;;                    (mu4e-drafts-folder . "/blarghmatey/[Gmail]/Drafts")
;;                    (mu4e-sent-folder . "/blarghmatey/[Gmail]/Sent Mail")
;;                    (mu4e-sent-messages-behavior . delete)
;;                    (smtpmail-smtp-user . "blarghmatey@gmail.com")
;;                    (smtpmail-smtp-server . "smtp.gmail.com")
;;                    (smtpmail-smtp-service . 587)
;;                    (mu4e-compose-dont-reply-to-self t)
;;                    (mu4e-compose-signature .
;;                                            (concat
;;                                             "Regards,\n"
;;                                             "Tobias Macey\n"
;;                                             "https://linkedin.com/in/tmacey\n"))
;;                    (mu4e-maildir-shortcuts .
;;                     (("/blarghmatey/Inbox" . ?i)
;;                      ("/blarghmatey/[Gmail]/Sent Mail" . ?s)
;;                      ("/blarghmatey/[Gmail]/Drafts" . ?d)))
;;                    ))
;;          ,(make-mu4e-context
;;            :name "tobiasmacey"
;;            :enter-func (lambda () (define-key mu4e-headers-mode-map (kbd "C-c C-a")
;;                                     (lambda ()
;;                                       (interactive)
;;                                       (let ((mu4e-get-mail-command "mbsync tobiasmacey"))
;;                                         (mu4e-update-mail-and-index t)))))
;;            :match-func (lambda (msg)
;;                          (when msg
;;                            (string-prefix-p "/tobiasmacey" (mu4e-message-field msg :maildir))))
;;            :vars '((user-mail-address . "tobias.macey@gmail.com")
;;                    (user-full-name . "Tobias Macey")
;;                    (mu4e-trash-folder . "/tobiasmacey/[Gmail]/Trash")
;;                    (mu4e-drafts-folder . "/tobiasmacey/[Gmail]/Drafts")
;;                    (mu4e-sent-folder . "/tobiasmacey/[Gmail]/Sent Mail")
;;                    (smtpmail-smtp-user . "tobias.macey@gmail.com")
;;                    (smtpmail-smtp-server . "smtp.gmail.com")
;;                    (smtpmail-smtp-service . 587)
;;                    (mu4e-compose-dont-reply-to-self . t)
;;                    (mu4e-compose-signature .
;;                                            (concat
;;                                             "Regards,\n"
;;                                             "Tobias Macey\n"
;;                                             "https://linkedin.com/in/tmacey\n"))
;;                    (mu4e-maildir-shortcuts .
;;                     (("/tobiasmacey/Inbox" . ?i)
;;                      ("/tobiasmacey/[Gmail]/Sent Mail" . ?s)
;;                      ("/tobiasmacey/[Gmail]/Drafts" . ?d)))
;;                    ))
;;          ,(make-mu4e-context
;;            :name "xboundlessnotions"
;;            :enter-func (lambda () (define-key mu4e-headers-mode-map (kbd "C-c C-a")
;;                                     (lambda ()
;;                                       (interactive)
;;                                       (let ((mu4e-get-mail-command "mbsync boundlessnotions"))
;;                                         (mu4e-update-mail-and-index t)))))
;;            :match-func (lambda (msg)
;;                          (when msg
;;                            (string-prefix-p "/boundlessnotions" (mu4e-message-field msg :maildir))))
;;            :vars '((user-mail-address . "tmacey@boundlessnotions.com")
;;                    (user-full-name . "Tobias Macey")
;;                    (mu4e-trash-folder . "/boundlessnotions/[Gmail]/Trash")
;;                    (mu4e-drafts-folder . "/boundlessnotions/[Gmail]/Drafts")
;;                    (mu4e-sent-folder . "/boundlessnotions/[Gmail]/Sent Mail")
;;                    (smtpmail-smtp-user . "tmacey@boundlessnotions.com")
;;                    (smtpmail-smtp-server . "smtp.gmail.com")
;;                    (smtpmail-smtp-service . 587)
;;                    (mu4e-compose-dont-reply-to-self . t)
;;                    (mu4e-compose-signature .
;;                                            (concat
;;                                             "Regards,\n"
;;                                             "Tobias Macey\n"
;;                                             "Owner and Chief Engineer\n"
;;                                             "Boundless Notions, LLC.\n"
;;                                             "https://www.boundlessnotions.com\n"))
;;                    (mu4e-maildir-shortcuts .
;;                     (("/boundlessnotions/Inbox" . ?i)
;;                      ("/boundlessnotions/[Gmail]/Sent Mail" . ?s)
;;                      ("/boundlessnotions/[Gmail]/Drafts" . ?d)))
;;                    ))
;;          ,(make-mu4e-context
;;            :name "zbitlancer"
;;            :enter-func (lambda () (define-key mu4e-headers-mode-map (kbd "C-c C-a")
;;                                     (lambda ()
;;                                       (interactive)
;;                                       (let ((mu4e-get-mail-command "mbsync bitlancer"))
;;                                         (mu4e-update-mail-and-index t)))))
;;            :match-func (lambda (msg)
;;                          (when msg
;;                            (string-prefix-p "/bitlancer" (mu4e-message-field msg :maildir))))
;;            :vars '((user-mail-address . "tmacey@bitlancer.com")
;;                    (user-full-name . "Tobias Macey")
;;                    (mu4e-trash-folder . "/bitlancer/[Gmail]/Trash")
;;                    (mu4e-drafts-folder . "/bitlancer/[Gmail]/Drafts")
;;                    (mu4e-sent-folder . "/bitlancer/[Gmail]/Sent Mail")
;;                    (smtpmail-smtp-user . "tmacey@bitlancer.com")
;;                    (smtpmail-smtp-server . "smtp.gmail.com")
;;                    (smtpmail-smtp-service . 587)
;;                    (mu4e-compose-dont-reply-to-self . t)
;;                    (mu4e-compose-signature .
;;                                            (concat
;;                                             "Regards,\n"
;;                                             "Tobias Macey\n"
;;                                             "Senior Cloud Architect\n"
;;                                             "Bitlancer LLC.\n"
;;                                             "https://www.bitlancer.com\n"))
;;                    (mu4e-maildir-shortcuts .
;;                     (("/bitlancer/Inbox" . ?i)
;;                      ("/bitlancer/[Gmail]/Sent Mail" . ?s)
;;                      ("/bitlancer/[Gmail]/Drafts" . ?d)))
;;                    ))
;;          ,(make-mu4e-context
;;            :name "podcastinit"
;;            :enter-func (lambda () (define-key mu4e-headers-mode-map (kbd "C-c C-a")
;;                                     (lambda ()
;;                                       (interactive)
;;                                       (let ((mu4e-get-mail-command "mbsync podcastinit"))
;;                                         (mu4e-update-mail-and-index t)))))
;;            :match-func (lambda (msg)
;;                          (when msg
;;                            (string-prefix-p "/podcastinit" (mu4e-message-field msg :maildir))))
;;            :vars '((user-mail-address . "tmacey@podcastinit.com")
;;                    (user-full-name . "Tobias Macey")
;;                    (mu4e-trash-folder . "/podcastinit/Trash")
;;                    (mu4e-drafts-folder . "/podcastinit/Drafts")
;;                    (mu4e-sent-folder . "/podcastinit/Sent")
;;                    (smtpmail-smtp-user . "tmacey@podcastinit.com")
;;                    (smtpmail-smtp-server . "smtp.zoho.com")
;;                    (smtpmail-smtp-service . 587)
;;                    (mu4e-compose-dont-reply-to-self . t)
;;                    (mu4e-compose-signature .
;;                                            (concat
;;                                             "Regards,\n"
;;                                             "Tobias Macey\n"
;;                                             "Host of Podcast.__init__\n"
;;                                             "The podcast about Python and the people who make it great!\n"
;;                                             "https://www.podcastinit.com\n"))
;;                    (mu4e-maildir-shortcuts .
;;                     (("/podcastinit/Inbox" . ?i)
;;                      ("/podcastinit/Sent" . ?s)
;;                      ("/podcastinit/Drafts" . ?d)))
;;                    ))
;;          ,(make-mu4e-context
;;            :name "dataengineering"
;;            :enter-func (lambda () (define-key mu4e-headers-mode-map (kbd "C-c C-a")
;;                                     (lambda ()
;;                                       (interactive)
;;                                       (let ((mu4e-get-mail-command "mbsync dataengineering"))
;;                                         (mu4e-update-mail-and-index t)))))
;;            :match-func (lambda (msg)
;;                          (when msg
;;                            (string-prefix-p "/dataengineering" (mu4e-message-field msg :maildir))))
;;            :vars '((user-mail-address . "tmacey@dataengineeringpodcast.com")
;;                    (user-full-name . "Tobias Macey")
;;                    (mu4e-trash-folder . "/dataengineering/Trash")
;;                    (mu4e-drafts-folder . "/dataengineering/Drafts")
;;                    (mu4e-sent-folder . "/dataengineering/Sent")
;;                    (smtpmail-smtp-user . "tmacey@dataengineeringpodcast.com")
;;                    (smtpmail-smtp-server . "smtp.migadu.com")
;;                    (smtpmail-smtp-service . 587)
;;                    (mu4e-compose-dont-reply-to-self . t)
;;                    (mu4e-compose-signature .
;;                                            (concat
;;                                             "Regards,\n"
;;                                             "Tobias Macey\n"
;;                                             "Host of the Data Engineering Podcast\n"
;;                                             "The podcast about modern data management and the people who make it possible.\n"
;;                                             "https://www.dataengineeringpodcast.com\n"))
;;                    (mu4e-maildir-shortcuts .
;;                     (("/dataengineering/Inbox" . ?i)
;;                      ("/dataengineering/Sent" . ?s)
;;                      ("/dataengineering/Drafts" . ?d)))
;;                    ))
;;          ,(make-mu4e-context
;;            :name "mitodl"
;;            :enter-func (lambda () (define-key mu4e-headers-mode-map (kbd "C-c C-a")
;;                                     (lambda ()
;;                                       (interactive)
;;                                       (let ((mu4e-get-mail-command "mbsync mitodl"))
;;                                         (mu4e-update-mail-and-index t)))))
;;            :match-func (lambda (msg)
;;                          (when msg
;;                            (string-prefix-p "/mitodl" (mu4e-message-field msg :maildir))))
;;            :vars '((user-mail-address . "tmacey@mit.edu")
;;                    (user-full-name . "Tobias Macey")
;;                    (mu4e-sent-folder . "/mitodl/Sent Items")
;;                    (mu4e-trash-folder . "/mitodl/Trash")
;;                    (mu4e-drafts-folder . "/mitodl/Drafts")
;;                    (smtpmail-smtp-user . "tmacey")
;;                    (smtpmail-smtp-server . "outgoing.mit.edu")
;;                    (smtpmail-smtp-service . 587)
;;                    (mu4e-compose-dont-reply-to-self . t)
;;                    (mu4e-compose-signature .
;;                                            (concat
;;                                             "Regards,\n"
;;                                             "Tobias Macey\n"
;;                                             "DevOps Engineering Manager\n"
;;                                             "MIT Open Learning\n"
;;                                             "https://openlearning.mit.edu\n"))
;;                    (mu4e-maildir-shortcuts .
;;                     (("/mitodl/Inbox" . ?i)
;;                      ("/mitodl/Sent Items" . ?s)
;;                      ("/mitodl/Drafts" . ?d)
;;                      ("/mitodl/Inbox/Django Errors" . ?e)))
;;                    ))
;;          ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
