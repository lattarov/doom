;;; private/work/config.el -*- lexical-binding: t; -*-

;;
;;; Org babel

(after! org
  (setopt org-babel-C++-compiler "clang"
          org-babel-C-compiler   "clang"))
