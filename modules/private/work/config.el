;;; private/work/config.el -*- lexical-binding: t; -*-

;;
;;; Org babel

(after! org
  (setopt org-babel-C++-compiler "clang"
          org-babel-C-compiler   "clang"))


;;
;;; TightVNC

(require 'transient)
(defvar vlv-vnc-machines
  '(("VISIO"            . "10.4.54.221")
    ("VERN"             . "10.102.31.63")
    ("CMX"              . "10.4.54.223")
    ("CXP"              . "10.4.54.224")
    ("STARLORD"         . "10.4.52.3")
    ("BDT-INCA10-VERN"  . "10.102.31.64"))
  "Alist mapping machine names to their TightVNC IPs.")

(defcustom vlv-vncviewer-path "/mnt/c/Program Files/TightVNC/tvnviewer.exe"
  "Path to the TightVNC viewer executable."
  :type 'file
  :group 'work)

(defun vlv--vnc-password ()
  "Retrieve the VNC password from auth-source.
Expects an entry for host \"vnc-work\" and user \"admin\"."
  (require 'auth-source)
  (let ((entry (car (auth-source-search :host "vnc-work" :user "admin" :max 1))))
    (unless entry
      (user-error "No auth-source entry for host vnc-work, user admin"))
    (let ((secret (plist-get entry :secret)))
      (if (functionp secret) (funcall secret) secret))))

(defun vlv--vnc-check-viewer ()
  "Signal an error if the TightVNC viewer executable is not found."
  (unless (file-executable-p vlv-vncviewer-path)
    (user-error "TightVNC not found at: %s" vlv-vncviewer-path)))

(defun vlv-connect-to-vnc-with-machine (machine)
  "Connect to MACHINE by name via TightVNC."
  (let ((ip (cdr (assoc machine vlv-vnc-machines))))
    (unless ip (user-error "Unknown machine: %s" machine))
    (vlv-connect-to-vnc-with-ip ip)))

(defun vlv-connect-to-vnc-with-ip (ip)
  "Connect to IP via TightVNC."
  (vlv--vnc-check-viewer)
  (let ((password (vlv--vnc-password)))
    (message "Connecting to %s..." ip)
    (start-process "tightvnc" nil vlv-vncviewer-path
                   ip (concat "-password=" password))))

(defun vlv--vnc-assign-keys (machines)
  "Assign unique single-char keys to MACHINES, falling back to digits on collision."
  (let ((used '())
        (counter 0))
    (mapcar (lambda (pair)
              (let* ((name (car pair))
                     (preferred (downcase (substring name 0 1)))
                     (key (if (member preferred used)
                              (prog1 (number-to-string counter)
                                (cl-incf counter))
                            preferred)))
                (push key used)
                (cons key name)))
            machines)))

(defun vlv--vnc-transient-items ()
  "Build transient suffix list dynamically from `vlv-vnc-machines'."
  (mapcar (lambda (pair)
            (let ((key  (car pair))
                  (name (cdr pair)))
              (list key name
                    `(lambda () (interactive)
                       (vlv-connect-to-vnc-with-machine ,name)))))
          (vlv--vnc-assign-keys vlv-vnc-machines)))

(transient-define-prefix vlv-vnc-menu ()
  "TightVNC machine selector — built dynamically from `vlv-vnc-machines'."
  [:description "Machines"
   :class transient-column
   :setup-children
   (lambda (_)
     (mapcar
      (lambda (item)
        (transient-parse-suffix 'vlv-vnc-menu item))
      (append
       (vlv--vnc-transient-items)
       '(("o" "Choose IP…"
          (lambda () (interactive)
            (vlv-connect-to-vnc-with-ip (read-string "IP: "))))))))])

(map! :prefix "C-c" :desc "TightVNC menu" "v" #'vlv-vnc-menu)


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
