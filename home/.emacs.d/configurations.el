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

(setq-default fill-column 120)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.html" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jinja" . jinja2-mode))
(add-to-list 'auto-mode-alist '("\\.sls" . yaml-mode))

(setq speedbar-indentation-width 2)
(setq speedbar-show-unknown-files t)
(setq speedbar-smart-directory-expand-flag nil)
(setq speedbar-use-images nil)
(setq sr-speedbar-right-side nil)
(setq sr-speednbar-skip-other-window-p t)

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
(setq split-width-threshold 70)
(setq split-height-threshold 100)
(setq apropos-sort-by-scores t)

(setq-default tab-width 4)
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)
(setq-default tab-always-indent 'complete)
(setq-default major-mode 'text-mode)
(setq indent-line-function 'indent-for-tab-command)

(setq magit-last-seen-setup-instructions "1.4.0")
(use-package magithub
  :after magit
  :ensure t
  :config (magithub-feature-autoinject t))

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
(setq default-frame-alist '((font . "Hack-10") (load-theme 'tsdh-dark)))
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
(global-set-key (kbd "C-c f p") (lambda () (interactive) (set-face-attribute 'default nil :font "Hack-14")))
(global-set-key (kbd "C-c f N") (lambda () (interactive) (set-face-attribute 'default nil :font "Hack-8")))
(global-set-key (kbd "C-c f m") (lambda () (interactive) (set-face-attribute 'default nil :font "Hack-10")))
(global-set-key (kbd "C-c f n") (lambda () (interactive) (set-face-attribute 'default nil :font "Hack-7")))
(global-set-key (kbd "C-c f t") (lambda () (interactive) (set-face-attribute 'default nil :font "Hack-5")))

;; Use C-c t as a prefix for toggling things
(global-set-key (kbd "C-c t l") 'linum-relative-toggle)

(global-set-key (kbd "C-c i d") 'insert-date)

(global-set-key (kbd "C-c e") 'eval-and-replace)
(global-set-key (kbd "C-x p") 'other-window-backward)

(global-set-key (kbd "C-c C-t") 'visit-ansi-term)
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

(global-set-key (kbd "M-s t") 'sr-speedbar-toggle)
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

(global-set-key (kbd "C-c C-\"") 'insert-pair)
(global-set-key (kbd "C-c C-'") 'insert-pair)
(global-set-key (kbd "C-c C-(") 'insert-pair)
(global-set-key (kbd "C-c C-{") 'insert-pair)
(global-set-key (kbd "C-c C-[") 'insert-pair)

(global-set-key (kbd "C-M-p") 'projectile-find-file)

(global-set-key (kbd "M-%") 'query-replace-regexp)

(global-set-key [f1] 'toggle-fold-to-signatures)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ORGMODE CONFIGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'org-trello)
;; (setq org-trello-current-prefix-keybinding "C-c o" nil (org-trello))
(require 'org-alert)
(require 'org-gcal)
(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))
(setq org-agenda-restore-windows-after-quit t)
(setq alert-default-style 'libnotify)
(setq org-alert-enable t)
(setq org-directory "~/Dropbox/org/")
(setq org-mobile-directory "~/Dropbox/org-mobile/")
(setq org-mobile-inbox-for-pull "~/Dropbox/org-mobile/inbox.org")
(setq org-log-done t)
(setq org-todo-keywords
      '((sequence "TODO" "DOING" "|" "DONE")))
(setq org-todo-keyword-faces
      '(("TODO" . org-warning) ("DOING" . "yellow") ("DONE" . (:foreground "green" :weight bold))))
(setq org-log-redeadline (quote time))
(setq org-log-reschedule (quote time))
(setq org-agenda-files
   (quote
    ("~/Dropbox/org/todo/" "~/Dropbox/org/calendars" "~/Dropbox/org/journal")))
(setq org-agenda-time-grid
      (quote
       ((daily weekly today require-timed)
        (800 1000 1200 1400 1600 1800 2000)
        "......"
        "----------------")))
(setq org-journal-date-format "%Y-%m-%d")
(setq org-journal-dir "~/Dropbox/org/journal/")
(setq org-journal-file-format "%Y-%m-%d.org")
(setq org-journal-search-results-order-by :desc)
(setq org-mobile-directory "~/Dropbox/org")
(setq org-default-notes-file (concat org-directory "notes.org"))
(setq org-refile-targets `((org-agenda-files . (:maxlevel . 2))))
(setq org-capture-templates
      `(("t" "Todo" entry (file ,(concat org-directory "todo/todo.org"))
         "** TODO %? :%^G\n:PROPERTIES:\n:Created: %U\n:END:")
        ("n" "Note" entry (file org-default-notes-file)
         "* %?\n:PROPERTIES:\n:Created: %U\n:END:")))
(ignore-errors (load-file "~/.org-gcal-client-secrets.el.gpg"))
(setq org-gcal-file-alist `(("blarghmatey@gmail.com" . ,(concat org-directory "calendars/blarghmatey.org"))
                            ("sabrinaleevt@gmail.com" . ,(concat org-directory "calendars/sabrinaleevt.org"))
                            ("tobias.macey@gmail.com" . ,(concat org-directory "calendars/tobiasmacey.org"))
                            ("cks7rp25je0uau65oiq6nh4trs@group.calendar.google.com" . ,(concat org-directory "calendars/mit-engineering.org"))
                            ("tmacey@bitlancer.com" . ,(concat org-directory "calendars/bitlancer.org"))
                            ("tmacey@boundlessnotions.com" . ,(concat org-directory "calendars/boundlessnotions.org"))))

(global-set-key (kbd "C-c L") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c o n") (lambda () (interactive) (find-file org-default-notes-file)))
(global-set-key (kbd "C-c o t") (lambda () (interactive) (find-file (concat org-directory "todo/"))))
(global-set-key (kbd "C-c s e") `org-edit-src-code)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EMAIL CONFIGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(require 'org-mu4e)
(require 'smtpmail)
(require 'mu4e-contrib)
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it)
(setq mu4e-maildir (expand-file-name "~/.mail"))
(setq mu4e-use-fancy-chars t)
(setq mu4e-view-show-images t)
(setq mu4e-show-images t)
(setq mu4e-html2text-command 'mu4e-shr2text)
(setq shr-use-colors nil)
(setq mu4e-get-mail-command "mbsync -a")
(setq mu4e-change-filenames-when-moving t)
(setq mu4e-completing-read-function 'completing-read)
(add-to-list 'mu4e-view-actions
             '("ViewInBrowser" . mu4e-action-view-in-browser) t)
(add-to-list 'mu4e-view-actions
             '("Eww view" . jcs-view-in-eww) t)
(add-to-list 'mu4e-headers-fields '(:maildir))
(define-key mu4e-view-mode-map "\t" 'shr-next-link)
(define-key mu4e-view-mode-map (kbd "<backtab>") 'shr-previous-link)
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))
(setq mu4e-compose-format-flowed t)
(setq mu4e-compose-dont-reply-to-self t)
(setq mu4e-update-interval 1200)
(setq mu4e-view-prefer-html t)
(setq mu4e-context-policy nil)
(setq message-kill-buffer-on-exit t)
(setq org-mu4e-convert-to-html t)
(setq mu4e-view-show-addresses t)
(setq mu43-scroll-to-next nil)
(setq mu4e-compose-dont-reply-to-self t)
(setq org-mime-export-options '(:section-numbers nil
                                :with-author nil
                                :with-toc nil))
(setq mu4e-user-mail-address-list '("tmacey@boundlessnotions.com"
                                    "tmacey@renaissancedev.com"
                                    "tmacey@bitlancer.com"
                                    "tmacey@mit.edu"
                                    "tmacey@podcastinit.com"
                                    "blarghmatey@gmail.com"
                                    "tobias.macey@gmail.com"))
(setq mu4e-contexts
      `( ,(make-mu4e-context
           :name "blarghmatey"
           :match-func (lambda (msg)
                         (when msg
                           (string-prefix-p "/blarghmatey" (mu4e-message-field msg :maildir))))
           :vars '((user-mail-address . "blarghmatey@gmail.com")
                   (mu4e-user-mail-address-list . ("tmacey@boundlessnotions.com"
                                                   "tmacey@renaissancedev.com"
                                                   "tmacey@bitlancer.com"
                                                   "tmacey@mit.edu"
                                                   "tmacey@podcastinit.com"
                                                   "blarghmatey@gmail.com"
                                                   "tobias.macey@gmail.com"))
                   (user-full-name . "Tobias Macey")
                   (mu4e-trash-folder . "/blarghmatey/[Gmail]/Trash")
                   (mu4e-drafts-folder . "/blarghmatey/[Gmail]/Drafts")
                   (mu4e-sent-folder . "/blarghmatey/[Gmail]/Sent Mail")
                   ;; (mu4e-get-mail-command . "mbsync blarghmatey")
                   (mu4e-sent-messages-behavior . delete)
                   (smtpmail-smtp-user . "blarghmatey@gmail.com")
                   (smtpmail-smtp-server . "smtp.gmail.com")
                   (smtpmail-smtp-service . 587)
                   (mu4e-compose-dont-reply-to-self t)
                   (mu4e-compose-signature .
                                           (concat
                                            "Regards,\n"
                                            "Tobias Macey\n"
                                            "https://linkedin.com/in/tmacey\n"))
                   (mu4e-maildir-shortcuts .
                    (("/blarghmatey/Inbox" . ?i)
                     ("/blarghmatey/[Gmail]/Sent Mail" . ?s)
                     ("/blarghmatey/[Gmail]/Drafts" . ?d)))
                   ))
         ,(make-mu4e-context
           :name "tobiasmacey"
           :match-func (lambda (msg)
                         (when msg
                           (string-prefix-p "/tobiasmacey" (mu4e-message-field msg :maildir))))
           :vars '((user-mail-address . "tobias.macey@gmail.com")
                   (mu4e-user-mail-address-list . ("tmacey@boundlessnotions.com"
                                                   "tmacey@renaissancedev.com"
                                                   "tmacey@bitlancer.com"
                                                   "tmacey@mit.edu"
                                                   "tmacey@podcastinit.com"
                                                   "blarghmatey@gmail.com"
                                                   "tobias.macey@gmail.com"))
                   (user-full-name . "Tobias Macey")
                   (mu4e-trash-folder . "/tobiasmacey/[Gmail]/Trash")
                   (mu4e-drafts-folder . "/tobiasmacey/[Gmail]/Drafts")
                   ;; (mu4e-get-mail-command . "mbsync tobiasmacey")
                   (smtpmail-smtp-user . "tobias.macey@gmail.com")
                   (smtpmail-smtp-server . "smtp.gmail.com")
                   (smtpmail-smtp-service . 587)
                   (mu4e-compose-dont-reply-to-self . t)
                   (mu4e-compose-signature .
                                           (concat
                                            "Regards,\n"
                                            "Tobias Macey\n"
                                            "https://linkedin.com/in/tmacey\n"))
                   (mu4e-maildir-shortcuts .
                    (("/tobiasmacey/Inbox" . ?i)
                     ("/tobiasmacey/[Gmail]/Sent Mail" . ?s)
                     ("/tobiasmacey/[Gmail]/Drafts" . ?d)))
                   ))
         ,(make-mu4e-context
           :name "xboundlessnotions"
           :match-func (lambda (msg)
                         (when msg
                           (string-prefix-p "/boundlessnotions" (mu4e-message-field msg :maildir))))
           :vars '((mu4e-user-mail-address-list . ("tmacey@boundlessnotions.com" "tmacey@renaissancedev.com"))
                   (mu4e-user-mail-address-list . ("tmacey@boundlessnotions.com"
                                                   "tmacey@renaissancedev.com"
                                                   "tmacey@bitlancer.com"
                                                   "tmacey@mit.edu"
                                                   "tmacey@podcastinit.com"
                                                   "blarghmatey@gmail.com"
                                                   "tobias.macey@gmail.com"))
                   (user-full-name . "Tobias Macey")
                   (mu4e-trash-folder . "/boundlessnotions/[Gmail]/Trash")
                   (mu4e-drafts-folder . "/boundlessnotions/[Gmail]/Drafts")
                   ;; (mu4e-get-mail-command . "mbsync boundlessnotions")
                   (smtpmail-smtp-user . "tmacey@boundlessnotions.com")
                   (smtpmail-smtp-server . "smtp.gmail.com")
                   (smtpmail-smtp-service . 587)
                   (mu4e-compose-dont-reply-to-self . t)
                   (mu4e-compose-signature .
                                           (concat
                                            "Regards,\n"
                                            "Tobias Macey\n"
                                            "Owner and Chief Engineer\n"
                                            "Boundless Notions, LLC.\n"
                                            "https://www.boundlessnotions.com\n"))
                   (mu4e-maildir-shortcuts .
                    (("/boundlessnotions/Inbox" . ?i)
                     ("/boundlessnotions/[Gmail]/Sent Mail" . ?s)
                     ("/boundlessnotions/[Gmail]/Drafts" . ?d)))
                   ))
         ,(make-mu4e-context
           :name "zbitlancer"
           :match-func (lambda (msg)
                         (when msg
                           (string-prefix-p "/bitlancer" (mu4e-message-field msg :maildir))))
           :vars '((user-mail-address . "tmacey@bitlancer.com")
                   (mu4e-user-mail-address-list . ("tmacey@boundlessnotions.com"
                                                   "tmacey@renaissancedev.com"
                                                   "tmacey@bitlancer.com"
                                                   "tmacey@mit.edu"
                                                   "tmacey@podcastinit.com"
                                                   "blarghmatey@gmail.com"
                                                   "tobias.macey@gmail.com"))
                   (user-full-name . "Tobias Macey")
                   (mu4e-trash-folder . "/bitlancer/[Gmail]/Trash")
                   (mu4e-drafts-folder . "/bitlancer/[Gmail]/Drafts")
                   ;; (mu4e-get-mail-command . "mbsync bitlancer")
                   (smtpmail-smtp-user . "tmacey@bitlancer.com")
                   (smtpmail-smtp-server . "smtp.gmail.com")
                   (smtpmail-smtp-service . 587)
                   (mu4e-compose-dont-reply-to-self . t)
                   (mu4e-compose-signature .
                                           (concat
                                            "Regards,\n"
                                            "Tobias Macey\n"
                                            "Senior Cloud Architect\n"
                                            "Bitlancer LLC.\n"
                                            "https://www.bitlancer.com\n"))
                   (mu4e-maildir-shortcuts .
                    (("/bitlancer/Inbox" . ?i)
                     ("/bitlancer/[Gmail]/Sent Mail" . ?s)
                     ("/bitlancer/[Gmail]/Drafts" . ?d)))
                   ))
         ,(make-mu4e-context
           :name "podcastinit"
           :match-func (lambda (msg)
                         (when msg
                           (string-prefix-p "/podcastinit" (mu4e-message-field msg :maildir))))
           :vars '((user-mail-address . "tmacey@podcastinit.com")
                   (mu4e-user-mail-address-list . ("tmacey@boundlessnotions.com"
                                                   "tmacey@renaissancedev.com"
                                                   "tmacey@bitlancer.com"
                                                   "tmacey@mit.edu"
                                                   "tmacey@podcastinit.com"
                                                   "blarghmatey@gmail.com"
                                                   "tobias.macey@gmail.com"))
                   (user-full-name . "Tobias Macey")
                   (mu4e-trash-folder . "/podcastinit/Trash")
                   (mu4e-drafts-folder . "/podcastinit/Drafts")
                   ;; (mu4e-get-mail-command . "mbsync podcastinit")
                   (smtpmail-smtp-user . "tmacey@podcastinit.com")
                   (smtpmail-smtp-server . "smtp.zoho.com")
                   (smtpmail-smtp-service . 587)
                   (mu4e-compose-dont-reply-to-self . t)
                   (mu4e-compose-signature .
                                           (concat
                                            "Regards,\n"
                                            "Tobias Macey\n"
                                            "Host of Podcast.__init__\n"
                                            "The podcast about Python and the people who make it great!\n"
                                            "https://www.podcastinit.com\n"))
                   (mu4e-maildir-shortcuts .
                    (("/podcastinit/Inbox" . ?i)
                     ("/podcastinit/Sent" . ?s)
                     ("/podcastinit/Drafts" . ?d)))
                   ))
         ,(make-mu4e-context
           :name "mitodl"
           :match-func (lambda (msg)
                         (when msg
                           (string-prefix-p "/mitodl" (mu4e-message-field msg :maildir))))
           :vars '((user-mail-address . "tmacey@mit.edu")
                   (mu4e-user-mail-address-list . ("tmacey@boundlessnotions.com"
                                                   "tmacey@renaissancedev.com"
                                                   "tmacey@bitlancer.com"
                                                   "tmacey@mit.edu"
                                                   "tmacey@podcastinit.com"
                                                   "blarghmatey@gmail.com"
                                                   "tobias.macey@gmail.com"))
                   (user-full-name . "Tobias Macey")
                   (mu4e-sent-folder . "/mitodl/Sent Items")
                   (mu4e-trash-folder . "/mitodl/Trash")
                   (mu4e-drafts-folder . "/mitodl/Drafts")
                   ;; (mu4e-get-mail-command "mbsync mitodl")
                   (smtpmail-smtp-user . "tmacey")
                   (smtpmail-smtp-server . "outgoing.mit.edu")
                   (smtpmail-smtp-service . 587)
                   (mu4e-compose-dont-reply-to-self . t)
                   (mu4e-compose-signature .
                                           (concat
                                            "Regards,\n"
                                            "Tobias Macey\n"
                                            "DevOps Engineering Manager\n"
                                            "MIT Office of Digital Learning\n"
                                            "https://engineering.odl.mit.edu\n"))
                   (mu4e-maildir-shortcuts .
                    (("/mitodl/Inbox" . ?i)
                     ("/mitodl/Sent Items" . ?s)
                     ("/mitodl/Drafts" . ?d)
                     ("/mitodl/Inbox/Django Errors" . ?e)))
                   ))
         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
