;;; package --- Summary
;;; Commentary:
;;; This file contains user defined functions to be loaded with
;;; the init.el file

;;; Code:
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

(defun google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
         (buffer-substring (region-beginning) (region-end))
         (read-string "Google: "))))))

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

(defun indent-defun ()
  "Indent the current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (region-beginning) (region-end))))

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

(defun installed-packages ()
  "Return the list of installed packages."
  (mapcar 'car package-alist))

;(defun save-package-list ()
  ;"Save the list of installed packages to a file."
  ;(interactive)
  ;(with-temp-file "~/Dropbox/.emacs-packages-installed.el"
    ;(insert (format "(defvar my-packages '%s)" (installed-packages)))))

;; Enable minor modes for given major modes
(defun default-minor-modes ()
  "Enable several minor modes that are generally applicable."
  (interactive)
  (rainbow-mode 1)
  (turn-on-ctags-auto-update-mode)
  (hexcolor-add-to-font-lock)
  )

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

;; Behave like vi's O command
(defun open-previous-line (arg)
  "Open a new line before the current one.
  See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))

(defun enable_flyspell ()
  "Set flyspell dictionary."
  (ispell-change-dictionary "american")
  (flyspell-prog-mode))

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

(defun other-window-backward (&optional n)
  "Select Nth previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

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
(defvar insert-date-format "%Y-%m-%d"
  "*Format for \\[insert-date] (c.f. 'format-time-string').")
(defvar insert-date-time-format "%Y-%m-%d %X"
  "*Format for \\[insert-date-time] (c.f. 'format-date-time-string').")

(defun insert-date ()
  "Insert the current date."
  (interactive "*")
  (insert (format-time-string insert-date-format
                              (current-time))))

(defun insert-time ()
  "Insert the current time."
  (interactive "*")
  (insert (format-time-string insert-time-format
                              (current-time))))

(defun insert-date-time ()
  "Insert the current date and time."
  (interactive "*")
  (insert (format-time-string insert-date-time-format
                              (current-time))))

(defun project-directory (buffer-name)
  "Returns the root directory of the project that contains the
given buffer. Any directory with a .git or .jedi file/directory
is considered to be a project root."
  (interactive)
  (let ((root-dir (file-name-directory buffer-name)))
    (while (and root-dir
                (not (file-exists-p (concat root-dir ".git")))
                (not (file-exists-p (concat root-dir ".hg")))
                (not (file-exists-p (concat root-dir ".jedi")))
                (not (file-exists-p (concat root-dir ".dir-locals.el")))
                (not (file-exists-p (concat root-dir ".env")))
                (not (file-exists-p (concat root-dir ".envrc")))
                (not (file-exists-p (concat root-dir "requirements.txt"))))
      (setq root-dir
            (if (equal root-dir "/")
                nil
              (file-name-directory (directory-file-name root-dir)))))
    root-dir))

(defun project-name (buffer-name)
  "Returns the name of the project that contains the given buffer."
  (let ((root-dir (project-directory buffer-name)))
    (if root-dir
        (file-name-nondirectory
         (directory-file-name root-dir))
      nil)))

(defun flycheck-python-set-executables ()
  (let ((exec-path (python-shell-calculate-exec-path)))
    (setq flycheck-python-pylint-executable (executable-find "pylint")
          flycheck-python-flake8-executable (executable-find "flake8")))
  ;; Force Flycheck mode on
  (flycheck-mode))

(defun flycheck-python-setup ()
  (add-hook 'hack-local-variables-hook 'flycheck-python-set-executables
            nil 'local))

;(defun load-packages ()
  ;"Load package list and install missing packages."
  ;(interactive)
  ;(load-file "~/Dropbox/.emacs-packages-installed.el")
  ;(dolist (p my-packages)
    ;(when (not (package-installed-p p))
      ;(package-install p))))



(defun toggle-fold-to-signatures ()
  "Toggle selective display of only function signatures."
  (interactive)
  (set-selective-display (if selective-display nil 1)))

(defun hexcolor-add-to-font-lock ()
  (font-lock-add-keywords nil hexcolor-keywords))


;; Jedi Mode
;; Function to find project root given a buffer
(defun get-project-root (buf repo-type init-file)
  (vc-find-root (expand-file-name (buffer-file-name buf)) repo-type))

(defvar jedi-config:find-root-function 'get-project-root)

;; And call this on initialization
(defun current-buffer-project-root ()
  (funcall jedi-config:find-root-function
           (current-buffer)
           jedi-config:vcs-root-sentinel
           jedi-config:python-module-sentinel))

(defun jedi-config:setup-server-args ()
  ;; little helper macro for building the arglist
  (defmacro add-args (arg-list arg-name arg-value)
    `(setq ,arg-list (append ,arg-list (list ,arg-name ,arg-value))))
  ;; and now define the args
  (let ((project-root (current-buffer-project-root)))

    (make-local-variable 'jedi:server-args)

    (when project-root
      (message (format "Adding system path: %s" project-root))
      (add-args jedi:server-args "--sys-path" project-root))

    (when jedi-config:with-virtualenv
      (message (format "Adding virtualenv: %s" jedi-config:with-virtualenv))
      (add-args jedi:server-args "--virtual-env" jedi-config:with-virtualenv))))

(require 'pyvenv)
(defun setup-venv ()
  "Activates the virtualenv of the current buffer."
  (interactive)
  (let ((project-title (project-name buffer-file-name)))
    (let ((members (member-ignore-case project-title (venv-get-candidates-dir "/home/tmacey/.virtualenvs"))))
      (if members
          (ignore-errors
            (pyvenv-workon project-title))))))

(defun python-2to3 ()
  "Convert current buffer from python 2 to python 3.
This command calls python3's script 「2to3」.  A backup will be generated by 「2to3」 and named “.bak”."
  (interactive)
  (let* (
         (fName (buffer-file-name))
         (fSuffix (file-name-extension fName))
         )
    (when (buffer-modified-p)
      (progn
        (when (y-or-n-p "Buffer modified. Do you want to save first?")
          (save-buffer) ) ) )

    (if (or (string-equal fSuffix "py") (string-equal fSuffix "py3") )
        (progn
          (shell-command (format "2to3 -w %s" fName))
          (revert-buffer  "IGNORE-AUTO" "NOCONFIRM" "PRESERVE-MODES")
          )
      (progn
        (error "file 「%s」 doesn't end in “.py” or “.py3”." fName)
        )
      )
    ))

(defun python-format-printf ()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "%[0-9.]*?[diouxXeEfFgGcrs]" nil t)
    (replace-match "{}"))
  (goto-char (point-min))
  (while (re-search-forward "%(\\([A-Za-z_.]+(*[A-Za-z_.]*)*\\))[0-9.]*?[diouxXeEfFgGcrs]" nil t)
    (replace-match "{\\1}"))
  (goto-char (point-min))
  (while (re-search-forward "\\(['\"].+['\"]\\)\\s-*%\\s-*
*\\s-*\\([-A-Za-z_.,
 	]+(*[-'\"A-Za-z%{}_.	 ,
]*)*['\"]?\\)\\()\\)?"
 nil t)
    (replace-match "\\1.format(\\2)\\3"))
  )

(provide 'functions)

(defvar trace-messages nil)
(defun enable-message-tracing ()
  (defadvice message (before who-said-that activate)
       "Find out who said that thing. and say so."
       (let ((trace nil) (n 1) (frame nil))
         (while (setq frame (backtrace-frame n))
           (setq n (1+ n)
                 trace (cons (cadr frame) trace)) )
         (ad-set-arg 0 (concat "<<%S>>:\n" (ad-get-arg 0)))
         (ad-set-args 1 (cons trace (ad-get-args 1))) )))

(defun disable-message-tracing ()
  (ad-disable-advice 'message 'before 'who-said-that)
       (ad-update 'message))

(defun toggle-message-tracing ()
  (interactive)
  (setq trace-messages (not trace-messages))
  (if trace-messages
      (enable-message-tracing)
    (disable-message-tracing)))

(defun jcs-view-in-eww (msg)
  (eww-browse-url (concat "file://" (mu4e~write-body-to-html msg))))


(defun find-next-unsafe-char (&optional coding-system)
  "Find the next character in the buffer that cannot be encoded by
coding-system. If coding-system is unspecified, default to the coding
system that would be used to save this buffer. With prefix argument,
prompt the user for a coding system."
  (interactive "Zcoding-system: ")
  (if (stringp coding-system) (setq coding-system (intern coding-system)))
  (if coding-system nil
    (setq coding-system
          (or save-buffer-coding-system buffer-file-coding-system)))
  (let ((found nil) (char nil) (csets nil) (safe nil))
    (setq safe (coding-system-get coding-system 'safe-chars))
    ;; some systems merely specify the charsets as ones they can encode:
    (setq csets (coding-system-get coding-system 'safe-charsets))
    (save-excursion
      ;;(message "zoom to <")
      (let ((end  (point-max))
            (here (point    ))
            (char  nil))
        (while (and (< here end) (not found))
          (setq char (char-after here))
          (if (or (eq safe t)
                  (< char ?\177)
                  (and safe  (aref safe char))
                  (and csets (memq (char-charset char) csets)))
              nil ;; safe char, noop
            (setq found (cons here char)))
          (setq here (1+ here))) ))
    (and found (goto-char (1+ (car found))))
    found))

(defun unpackaged/sort-sexps (beg end)
  "Sort sexps in region.
Comments stay with the code below."
  (interactive "r")
  (cl-flet ((skip-whitespace () (while (looking-at (rx (1+ (or space "\n"))))
                                  (goto-char (match-end 0))))
            (skip-both () (while (cond ((or (nth 4 (syntax-ppss))
                                            (ignore-errors
                                              (save-excursion
                                                (forward-char 1)
                                                (nth 4 (syntax-ppss)))))
                                        (forward-line 1))
                                       ((looking-at (rx (1+ (or space "\n"))))
                                        (goto-char (match-end 0)))))))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char beg)
        (skip-both)
        (cl-destructuring-bind (sexps markers)
            (cl-loop do (skip-whitespace)
                     for start = (point-marker)
                     for sexp = (ignore-errors
                                  (read (current-buffer)))
                     for end = (point-marker)
                     while sexp
                     ;; Collect the real string, then one used for sorting.
                     collect (cons (buffer-substring (marker-position start) (marker-position end))
                                   (save-excursion
                                     (goto-char (marker-position start))
                                     (skip-both)
                                     (buffer-substring (point) (marker-position end))))
                     into sexps
                     collect (cons start end)
                     into markers
                     finally return (list sexps markers))
          (setq sexps (sort sexps (lambda (a b)
                                    (string< (cdr a) (cdr b)))))
          (cl-loop for (real . sort) in sexps
                   for (start . end) in markers
                   do (progn
                        (goto-char (marker-position start))
                        (insert-before-markers real)
                        (delete-region (point) (marker-position end)))))))))
;;; functions.el ends here
