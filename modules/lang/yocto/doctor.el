;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/yocto/doctor.el

(unless (and (fboundp 'treesit-available-p) (treesit-available-p))
  (warn! "Emacs was built without tree-sitter; bitbake-ts-mode won't work"))

(when (and (fboundp 'treesit-language-available-p)
           (not (treesit-language-available-p 'bitbake)))
  (warn! "The bitbake tree-sitter grammar isn't installed. Run M-x treesit-install-language-grammar RET bitbake"))
