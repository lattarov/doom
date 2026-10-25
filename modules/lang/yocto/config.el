;;; lang/yocto/config.el -*- lexical-binding: t; -*-

;;
;;; BitBake

(add-to-list 'auto-mode-alist '("\\.bb\\'"       . bitbake-ts-mode))
(add-to-list 'auto-mode-alist '("\\.bbappend\\'" . bitbake-ts-mode))
(add-to-list 'auto-mode-alist '("\\.bbclass\\'"  . bitbake-ts-mode))

;; .inc is far too common an extension to claim globally; only treat it as
;; BitBake when the file lives inside a layer.
(add-to-list 'magic-mode-alist '(+yocto-layer-include-p . bitbake-ts-mode))

(after! treesit
  ;; not in `treesit-language-source-alist' by default
  (add-to-list 'treesit-language-source-alist
               '(bitbake "https://github.com/tree-sitter-grammars/tree-sitter-bitbake")))
