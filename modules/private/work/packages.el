;; -*- no-byte-compile: t; -*-
;;; private/work/packages.el

(package! consult-everything
  :pin "ac574a63b3e869787bbe6354b57a7a8021647ee3"
  :recipe '(:host github
            :repo "jthaman/consult-everything"
            :build t))

(package! alarm-clock)

(package! robot-mode)
(package! robot-log)

;; TODO switch to the GitHub recipe once published
(package! tightvnc-menu
  :recipe (:local-repo "~/dev/tightvnc" :files ("tightvnc.el")))
