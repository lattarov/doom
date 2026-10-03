;;; lang/yocto/config.el -*- lexical-binding: t; -*-

(defvar +yocto-snippets-dir (dir! "snippets/")
  "Directory holding the snippets shipped with this module.")


;;
;;; BitBake

(add-to-list 'auto-mode-alist '("\\.bb\\'"       . bitbake-ts-mode))
(add-to-list 'auto-mode-alist '("\\.bbappend\\'" . bitbake-ts-mode))
(add-to-list 'auto-mode-alist '("\\.bbclass\\'"  . bitbake-ts-mode))

;; .inc is far too common an extension to claim globally; only treat it as
;; BitBake when the file lives inside a layer.
(add-to-list 'magic-mode-alist '(+yocto-layer-include-p . bitbake-ts-mode))

(after! bitbake
  ;; bitbake.el claims every .bb*/.inc file for `bitbake-mode' once loaded,
  ;; shadowing the entries above.  Layer .inc files are handled by
  ;; `magic-mode-alist' instead.
  (setq auto-mode-alist (assoc-delete-all bitbake-mode-file-regex auto-mode-alist)))

(after! treesit
  ;; neither grammar is in `treesit-language-source-alist' by default
  (add-to-list 'treesit-language-source-alist
               '(bitbake "https://github.com/tree-sitter-grammars/tree-sitter-bitbake"))
  (add-to-list 'treesit-language-source-alist
               '(devicetree "https://github.com/joelspadin/tree-sitter-devicetree")))


;;
;;; Snippets

(when (modulep! :editor snippets)
  (after! yasnippet
    (add-to-list 'yas-snippet-dirs '+yocto-snippets-dir 'append #'eq)))
