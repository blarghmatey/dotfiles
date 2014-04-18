;;; package ---- Summary
;;; Commentary:

;;; Code:
(require 'package)
(add-to-list 'package-archives
   '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
   '("marmalade" . "http://marmalade-repo.org/packages/") t)
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)
(package-initialize)

(defun save-package-list ()
  (with-temp-file "~/Dropbox/.emacs-packages-installed.el" (insert (format "(defvar my-packages '%s)" package-activated-list))))

(add-hook 'kill-emacs-hook 'save-package-list)

(load-file "~/Dropbox/.emacs-packages-installed.el")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(global-auto-revert-mode t)

(autoload 'dirtree "dirtree" "Add directory to tree view" t)
(elscreen-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("ef365fdcbc9da94cb2558f66e6af18fb2c09b1a843e9db83b71b056e9c5bb8b0" default)))
 '(speedbar-indentation-width 2)
 '(speedbar-show-unknown-files t)
 '(speedbar-smart-directory-expand-flag nil)
 '(speedbar-use-images nil)
 '(sr-speedbar-right-side nil)
 '(sr-speedbar-skip-other-window-p t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(global-linum-mode 1) ; Show line numbers
(column-number-mode 1) ; Show cursor column position
(desktop-save-mode 1) ; Save/restore opened buffers

;; Enable minor modes for given major modes
(defun default-minor-modes ()
  "Enable several minor modes that are generally applicable."
  (interactive)
  (flycheck-mode)
  (helm-mode)
  (rainbow-mode)
  )
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'program-mode-hook 'default-minor-modes)
(add-hook 'web-mode 'default-minor-modes)
(add-hook 'python-mode-hook 'default-minor-modes)
(add-hook 'ruby-mode-hook 'default-minor-modes)
(add-hook 'emacs-lisp-mode-hook 'default-minor-modes)
(add-hook 'javascript-mode-hook 'default-minor-modes)
(add-hook 'javascript-mode-hook 'tern-mode)

(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay t)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Set font and font-size
(set-face-attribute 'default nil :font "Source Code Pro-9")

;; Autoindent open-*-lines
(defvar newline-and-indent t
  "Modify the behavior of the open-*-line functions to cause them to autoindent.")

;; Behave like vi's o command
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
  See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))
(global-set-key (kbd "C-o") 'open-next-line)
;; Behave like vi's O command
(defun open-previous-line (arg)
  "Open a new line before the current one. 
  See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))
(global-set-key (kbd "M-o") 'open-previous-line)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;   CONFIG FROM PETER EDDY BELOW ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs customizations I use everywhere
;;
;; To Use:
;;  (setq load-path (append load-path '("~/path/to/dir/containing/this-file")))
;;  (require 'pje-customizations)




(when (not package-archive-contents)
  (package-refresh-contents))

;;
;; General UI Stuff
;;

(scroll-bar-mode -1)
(tool-bar-mode -1)
(electric-indent-mode +1)

(defun enable_flyspell ()
  "Set flyspell dictionary."
  (ispell-change-dictionary "american")
  (flyspell-prog-mode))

;;
;; Lisp/Slime
;;

;;(setq slime-lisp-implementations
;;      '((ccl ("/usr/local/bin/ccl64" "--quiet"))
;;        (sbcl ("/usr/local/bin/sbcl") :coding-system utf-8-unix)))

;;
;; Groovy
;;

;;(add-to-list 'auto-mode-alist '("\\.groovy" . groovy-mode))
;;(add-to-list 'auto-mode-alist '("\\.gradle" . groovy-mode))

;; JavaScript mode indent 
;;(setq js-indent-level 2)

;; Magit mode for git
;(require 'magit)

; To automatically enter closing pair when opening pair is entered
(electric-pair-mode +1)

;; Ruby
;; Don't indent parameters inside parens more than normal 
(defvar ruby-deep-indent-paren nil)
;; enable subword (CamelCase-aware) just in ruby-mode
(add-hook 'ruby-mode-hook 'subword-mode)

;; Java mode

;(defun my-java-mode-hook ()
;  (setq c-basic-offset 2))

;(add-hook 'java-mode-hook 'my-java-mode-hook)
;(add-hook 'java-mode-hook 'subword-mode)

;; Don't split window horizontially
;(setq split-height-threshold 0)
;(setq split-width-threshold nil)

;(put 'narrow-to-region 'disabled nil)

;; Turn off auto-fill-mode in html mode
;(add-hook 'html-mode-hook (lambda () (auto-fill-mode -1)))

;; Make tab width 4 chars
(setq tab-width 4)

;; Turn on column number mode
;(column-number-mode)

;; Don't minimize emacs with Ctrl-Z
;; (global-unset-key (kbd "C-z"))

;; Completion that uses many different methods to find options.
;(global-set-key (kbd "M-/") 'hippie-expand)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)



;; Show matching parens
(show-paren-mode)

;; Allow direct edting of permission flags in wdired
(defvar wdired-allow-to-change-permissions t)

;; Don't ask to delete excess versions of files
(defvar trim-versions-without-asking t)

(require 'ido)
(ido-mode t)

;(eval-after-load 'grep
;  '(when (boundp 'grep-find-ignored-files)
;     (add-to-list 'grep-find-ignored-files "*.class")))

(defun func-region (start end func)
  "Run a function over the region between START and END in current buffer."
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (funcall func text)))))

(defun urlencode (start end)
  "Urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-hexify-string))

(defun urldecode (start end)
  "De-urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-unhex-string))

;; XML pretty printer, requires nXML mode
(defun pretty-print-xml-region (begin end)
  "Pretty format XML markup in selected region.  You need to have 'nxml-mode'
\(http://www.emacswiki.org/cgi-bin/wiki/NxmlMode) installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Ah, much better!"))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-c e") 'eval-and-replace)

;; ---------------------
;; Org Mode
;; ---------------------

;; Org Mode default org directory
;(setq org-directory "~/Dropbox/org")

;;

(defun other-window-backward (&optional n)
  "Select Nth previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

;(global-set-key (kbd "C-x n") 'other-window)
(global-set-key (kbd "C-x p") 'other-window-backward)

;; Note that the functions switch-to-buffer-other-window and
;; switch-to-buffer-other-frame are not currently similarlly advised:
(defadvice switch-to-buffer (before existing-buffer 
				    activate compile)
  "When interactive, switch to existing buffers only,
unless given a prefix argument.  Prevents unintentionally
creating buffers."
  (interactive
   (list (read-buffer "Switch to buffer:"
		      (other-buffer)
		      (null current-prefix-arg)))))

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

(defun unscroll-maybe-remember ()
  (if (not (get last-command 'unscrollable))
      (progn 
	(set-marker unscroll-point (point))
	(set-marker unscroll-window-start (window-start))
	(setq unscroll-hscroll (window-hscroll)))))

(defadvice scroll-up (before remember-for-unscroll
			     activate compile)
  "Remember where we started from, for 'unscroll'."
  (unscroll-maybe-remember))

(defadvice scroll-down (before remember-for-unscroll
			     activate compile)
  "Remember where we started from, for 'unscroll'."
  (unscroll-maybe-remember))

(defadvice scroll-left (before remember-for-unscroll
			     activate compile)
  "Remember where we started from, for 'unscroll'."
  (unscroll-maybe-remember))

(defadvice scroll-right (before remember-for-unscroll
			     activate compile)
  "Remember where we started from, for 'unscroll'."
  (unscroll-maybe-remember))

(defun unscroll ()
  "Jump to location specified by 'unscroll-to'."
  (interactive)
  (goto-char unscroll-point)
  (set-window-start nil unscroll-window-start)
  (set-window-hscroll nil unscroll-hscroll))

(defvar insert-time-format "%X"
  "*Format for \\[insert-time] (c.f. 'format-time-string').")
(defvar insert-date-format "%Y/%m/%d"
  "*Format for \\[insert-date] (c.f. 'format-time-string').")
(defvar insert-date-time-format "%Y/%m/%d %X"
  "*Format for \\[insert-date-time] (c.f. 'format-date-time-string').")

(defun insert-date ()
  "Insert the current date"
  (interactive "*")
  (insert (format-time-string insert-date-format 
			      (current-time))))

(defun insert-time ()
  "Insert the current time"
  (interactive "*")
  (insert (format-time-string insert-time-format 
			      (current-time))))

(defun insert-date-time ()
  "Insert the current date and time"
  (interactive "*")
  (insert (format-time-string insert-date-time-format
							  (current-time))))

;;
;; Common Lisp
;;

;; Don't ask about variable settings in lisp files
;(setq enable-local-variables nil)

;; ---------------------
;; OSX Clipboard stuff
;; ---------------------

;(defun setup-osx-clipboard-mods ()
  ;; Don't have emacs kills to go to sytem clipboard because it defeats
  ;; the usefullness of ClipMenu
;  (setq interprogram-cut-function nil)
  ;; Don't use C-y (yank) to paste from system clipboard, use Command-V instead
;  (setq interprogram-paste-function nil)

;  (defun paste-from-pasteboard ()
;    (interactive)
;    (and mark-active (filter-buffer-substring (region-beginning) (region-end) t))
;    (insert (ns-get-pasteboard)))

;  (defun copy-to-pasteboard (p1 p2)
;    (interactive "r*")
;    (ns-set-pasteboard (buffer-substring p1 p2))
;    (message "Copied selection to pasteboard"))

;  (defun cut-to-pasteboard (p1 p2)
;    (interactive "r*") (ns-set-pasteboard (filter-buffer-substring p1 p2 t)))

;  (global-set-key (kbd "s-v") 'paste-from-pasteboard)
;  (global-set-key (kbd "s-c") 'copy-to-pasteboard)
;  (global-set-key (kbd "s-x") 'cut-to-pasteboard))

;;
;; Window System Config
;;

(when window-system
  (blink-cursor-mode 0))

;;
;; OS-Dependant Config
;;

;; OSX
;(when (eq system-type 'darwin)
;  (setup-osx-clipboard-mods))

;; Start emacsserver if not yet running
;(unless (server-running-p)
;  (server-start))

; Disable emacs starter kit highlighting of current line
;(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

(require 'term)

;; (defun visit-term-buffer ()
;;   "Create or visit a ansi terminal buffer."
;;   (interactive)
;;   (if (not (get-buffer "*ansi-term*"))
;;       (progn
;;        (split-window-sensibly (selected-window))
;;        (other-window 1)
;;        (ansi-term (getenv "SHELL")))
;;       (switch-to-buffer-other-window "*ansi-term*")))

;(global-set-key (kbd "C-c t") 'visit-term-buffer)

(defun visit-ansi-term ()
    "If the current buffer is:
       1) a running ansi-term named *ansi-term*, rename it.
       2) a stopped ansi-term, kill it and create a new one.
       3) a non ansi-term, go to an already running ansi-term
          or start a new one while killing a defunt one"
    (interactive)
    (let ((is-term (string= "term-mode" major-mode))
          (is-running (term-check-proc (buffer-name)))
          (term-cmd (getenv "SHELL"))
          (anon-term (get-buffer "*ansi-term*")))
      (if is-term
          (if is-running
              (if (string= "*ansi-term*" (buffer-name))
                  (call-interactively 'rename-buffer)
                (if anon-term
                    (switch-to-buffer "*ansi-term*")
                  (ansi-term term-cmd)))
            (kill-buffer (buffer-name))
            (ansi-term term-cmd))
        (if anon-term
            (if (term-check-proc "*ansi-term*")
                (switch-to-buffer "*ansi-term*")
              (kill-buffer "*ansi-term*")
              (ansi-term term-cmd))
          (ansi-term term-cmd)))))

(global-set-key (kbd "C-c t") 'visit-ansi-term)

(defun google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
         (buffer-substring (region-beginning) (region-end))
         (read-string "Google: "))))))

(global-set-key (kbd "C-c g") 'google)

;(defun open-with ()
;  "Simple function that allows us to open the underlying
;file of a buffer in an external program."
;  (interactive)
;  (when buffer-file-name
;    (shell-command (concat
;                    (if (eq system-type 'darwin)
;                        "open"
;                      (read-shell-command "Open current file with: "))
;                    " "
;                    buffer-file-name))))
;
;(global-set-key (kbd "C-c o") 'open-with)

(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting, remove from
version control if file is under version control."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(global-set-key (kbd "C-c D")  'delete-file-and-buffer)

(defun indent-defun ()
  "Indent the current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (region-beginning) (region-end))))

(global-set-key (kbd "C-M-z") 'indent-defun)

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)))))))

(defvar quote-xml
   [?\C-s ?< ?\C-m ?\C-b ?\" ?\C-e ?\" ?\S-  ?+ ?\C-a ?\C-n])

(provide '.emacs)
;;; .emacs ends here
