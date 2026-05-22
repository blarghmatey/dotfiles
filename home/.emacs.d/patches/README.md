# Emacs Patches

This directory contains maintenance patches for Emacs configuration that fix bugs or work around issues in specific Emacs versions.

## Current Patches

### `python-treesit-fixes.el`

**Status:** Active for Emacs 30.2
**Created:** 2025-05-02
**Purpose:** Fix tree-sitter font-lock query error for Python

**Issue:**
Emacs 30.2's `python--treesit-keywords` includes multi-word operators (`"not in"`, `"is not"`) which cause `treesit-query-error` because tree-sitter query syntax doesn't support spaces inside string literals.

This results in errors during buffer editing:
```
Error during redisplay: (jit-lock-function 1) signaled
(treesit-query-error "Syntax error at" 358 ...)
```

**Solution:**
The patch overrides the broken font-lock rules by:
1. Removing multi-word operators from the keyword list
2. Keeping all single-word keywords and operators
3. Preserving all other font-lock features (strings, comments, etc.)
4. Only activating on Emacs 30.2 (conditionally checks version)

**Status Tracking:**
- ✅ Emacs 30.2: Bug confirmed, patch applied
- ❓ Emacs 30.3+: Unknown if fixed in newer versions

**Future Updates:**
When Emacs 30.3+ is released:
1. Test if bug is fixed in newer version
2. Update version check in `python-treesit-fixes.el` to limit to 30.2 only
3. Consider removing patch if fixed upstream

## Patch Loading

Patches are loaded in `~/.emacs.d/init.el` in the `treesit` use-package config:

```elisp
(load (expand-file-name "patches/python-treesit-fixes" user-emacs-directory) :noerror :nomessage)
```

All patches should:
- Load with `:noerror :nomessage` flags
- Include version/availability checks in the patch file itself
- Include comprehensive documentation explaining the issue and solution
