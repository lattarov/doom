;;; private/work/config.el -*- lexical-binding: t; -*-

;;
;;; Org babel

(after! org
  (setopt org-babel-C++-compiler "clang"
          org-babel-C-compiler   "clang"))


;;
;;; TightVNC

(use-package! tightvnc
  :defer t
  :init
  (map! :prefix "C-c" :desc "TightVNC menu" "v" #'tightvnc)
  :config
  (setopt tightvnc-machines
          '(("VISIO"           . "10.4.54.221")
            ("VERN"            . "10.102.31.63")
            ("CMX"             . "10.4.54.223")
            ("CXP"             . "10.4.54.224")
            ("STARLORD"        . "10.4.52.3")
            ("BDT-INCA10-VERN" . "10.102.31.64"))
          ;; one shared ~/.authinfo.gpg entry for every machine
          tightvnc-auth-source-host "vnc-work"
          tightvnc-auth-source-user "admin"))


;;
;;; Wind River Workbench

(defcustom vlv--workbench3-path
  "/mnt/c/WindRiver_6947/workbench-3.3/wrwb/platform/x86-win32/eclipse/wrwb-x86-win32.exe"
  "The path to the executable or script to start WindRiver 3 workbench IDE."
  :type 'file
  :group 'work)

(defcustom vlv--workbench4-path
  "/mnt/c/089-VXWORKS_CALIA4/workbench-4/startWorkbench.bat"
  "The path to the executable or script to start WindRiver 4 workbench IDE."
  :type 'file
  :group 'work)

(defun vlv-open-windriver3-workbench ()
  "Open Wind River Workbench 3 IDE."
  (interactive)
  (start-process "workbench" nil vlv--workbench3-path))

(defun vlv-open-windriver4-workbench ()
  "Open Wind River Workbench 4 IDE."
  (interactive)
  (start-process "workbench" nil vlv--workbench4-path))


(map! :leader
      :prefix "o"
      :desc "WindRiver Workbench 3" "W" #'vlv-open-windriver3-workbench)

(map! :leader
      :prefix "o"
      :desc "WindRiver Workbench 4" "w" #'vlv-open-windriver4-workbench)


;;
;;; consult-everything (Windows search)

(after! consult-everything
  ;; the .exe makes it work on wsl
  (setopt consult-everything-args "es.exe -r"))

;; put here to avoid byte-compiler errors in git-commit-hook
(map! :leader
      :after consult-everything
      :prefix "s"
      :desc "consult-everything" "E" #'consult-everything)


;;
;;; Bureaucracy

(defun vlv-env-work-gen-payfit-report (ON-SITE-DAYS HOME-OFFICE-DAYS &optional COMMENT?)
  "Create the monthly comment message for payfit.

ON-SITE-DAYS and HOME-OFFICE-DAYS are self descriptive.
COMMENT is an optional string appended to a new line."
  (interactive "nOn site days: \nnHome Office days:")
  (let* ((on-site-fee (* ON-SITE-DAYS 15))
         (home-office-fee (* HOME-OFFICE-DAYS 10))
         (total (+ on-site-fee home-office-fee))
         (comment (if COMMENT? (format " \n %s" COMMENT?) ""))
         (text (format "%d jours x 15€/jour sur site + %d jours x 10€/jour TT = %d€ + %d€ = %d%s€"
                       ON-SITE-DAYS HOME-OFFICE-DAYS on-site-fee home-office-fee total comment)))
    
                                        ; send the text to clipboard and log
    (kill-new text)
    (message "sent to clipboard: \"%s\"" text)))


;;
;;; Alarms

(after! alarm-clock
  (alarm-clock-set "08:30" "🍵 Coffee time!")
  (alarm-clock-set "10:00" "🍵 Coffee time!")
  (alarm-clock-set "12:00" "nham nham!")
  (alarm-clock-set "15:00" "🍵 Coffee time!")
  (alarm-clock-set "17:00" "Go home!"))
