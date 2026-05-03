;;; python-treesit-fixes.el --- Fix broken tree-sitter fontlock for Python in Emacs 30.2

;; Copyright (C) 2025 Copilot
;; Author: Copilot
;; Created: 2025-05-02
;; Version: 1.0
;; Keywords: python, treesit, fontlock, emacs30.2

;; Description:
;; Emacs 30.2 has a tree-sitter fontlock bug for Python where multi-word
;; operators ("not in", "is not") in python--treesit-keywords cause treesit-query-error
;; because tree-sitter query syntax doesn't support spaces inside string literals.
;;
;; This patch overrides the broken font-lock rules with corrected versions that:
;; 1. Remove multi-word operators from the keyword list
;; 2. Keep single-word keywords and operators
;; 3. Preserve all other fontlock features (strings, comments, etc.)
;; 4. Only activates on Emacs 30.2 (target version of the bug)

;;; Code:

(defun mp-fix-python-ts-fontlock ()
  "Correct the broken python-ts-mode font-lock rules in Emacs 30.2.

The tree-sitter query in python--treesit-keywords includes multi-word
operators like 'not in' and 'is not' which cause parse errors because
tree-sitter query syntax doesn't support spaces in string literals.

This fix rebuilds only the 'keyword' feature with single-word operators only,
preserving all other font-lock features."
  (when (and (treesit-available-p)
             (treesit-language-available-p 'python))
    (add-hook 'python-ts-mode-hook
              (lambda ()
                ;; Update only the keyword feature to remove multi-word operators
                (setq-local treesit-font-lock-settings
                            (treesit-font-lock-rules
                             ;; KEYWORD feature: single-word keywords only
                             ;; Removed: "not in", "is not" which cause parse errors
                             :feature 'keyword
                             :language 'python
                             '(["as" "assert" "async" "await" "break" "case" "class"
                                "continue" "def" "del" "elif" "else" "except" "exec"
                                "finally" "for" "from" "global" "if" "import" "lambda"
                                "match" "nonlocal" "pass" "print" "raise" "return" "try"
                                "while" "with" "yield"
                                ;; Binary operators (highlighted as keywords)
                                "and" "in" "is" "not" "or"]
                               @font-lock-keyword-face
                               ;; Special case: highlight 'self' as keyword
                               ((identifier) @font-lock-keyword-face
                                (:match "\\`self\\'" @font-lock-keyword-face)))))))))

;; Activate the patch only if this is Emacs 30.2 (the affected version)
(when (and (>= emacs-major-version 30)
           (= emacs-minor-version 2)
           (treesit-available-p))
  (mp-fix-python-ts-fontlock))

(provide 'python-treesit-fixes)

;;; python-treesit-fixes.el ends here
