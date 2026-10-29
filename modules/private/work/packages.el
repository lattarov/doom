;; -*- no-byte-compile: t; -*-
;;; private/work/packages.el

(package! consult-everything
  :pin "ac574a63b3e869787bbe6354b57a7a8021647ee3"
  :recipe (:host github :repo "jthaman/consult-everything"))

(package! alarm-clock)

(package! robot-mode)
(package! robot-log)

(package! tightvnc
  :recipe (:host github :repo "lattarov/tightvnc.el"))
