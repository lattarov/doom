;;; lang/yocto/autoload/yocto.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +yocto-layer-root (&optional file)
  "Return the root of the Yocto layer containing FILE, or nil.

FILE defaults to the file visited by the current buffer.  A layer is any
directory holding a conf/layer.conf."
  (when-let* ((file (or file buffer-file-name)))
    (locate-dominating-file file "conf/layer.conf")))

;;;###autoload
(defun +yocto-layer-include-p ()
  "Return non-nil if the current buffer visits a .inc file inside a Yocto layer.

Meant for `magic-mode-alist', so .inc files elsewhere (C, PHP, Pascal...)
keep their usual major mode."
  (and buffer-file-name
       (string-match-p "\\.inc\\'" buffer-file-name)
       (+yocto-layer-root buffer-file-name)
       t))
