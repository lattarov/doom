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
  "Retrieve VNC password from auth-source, or prompt if not found."
  (require 'auth-source)
  (let ((entry (auth-source-search :host "vnc-work" :user "admin" :max 1)))
    (if entry
        (funcall (plist-get (car entry) :secret))
      "ADMINVME")))

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
  (message "Connecting to %s..." ip)
  (start-process "tightvnc" nil vlv-vncviewer-path
                 ip (concat "-password=" (vlv--vnc-password))))

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
