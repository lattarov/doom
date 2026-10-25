;;; private/work/config.el -*- lexical-binding: t; -*-

(defgroup vlv-work nil
  "Work-only configuration."
  :group 'convenience)


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

(defcustom vlv-work-workbench3-path
  "/mnt/c/WindRiver_6947/workbench-3.3/wrwb/platform/x86-win32/eclipse/wrwb-x86-win32.exe"
  "The path to the executable or script to start WindRiver 3 workbench IDE."
  :type 'file
  :group 'vlv-work)

(defcustom vlv-work-workbench4-path
  "/mnt/c/089-VXWORKS_CALIA4/workbench-4/startWorkbench.bat"
  "The path to the executable or script to start WindRiver 4 workbench IDE."
  :type 'file
  :group 'vlv-work)

(defun vlv-work-open-windriver3-workbench ()
  "Open Wind River Workbench 3 IDE."
  (interactive)
  (start-process "workbench" nil vlv-work-workbench3-path))

(defun vlv-work-open-windriver4-workbench ()
  "Open Wind River Workbench 4 IDE."
  (interactive)
  (start-process "workbench" nil vlv-work-workbench4-path))

(map! :leader
      :prefix "o"
      :desc "WindRiver Workbench 3" "W" #'vlv-work-open-windriver3-workbench
      :desc "WindRiver Workbench 4" "w" #'vlv-work-open-windriver4-workbench)


;;
;;; consult-everything (Windows search)

(after! consult-everything
  ;; the .exe makes it work on wsl
  (setopt consult-everything-args "es.exe -r"))

(map! :leader
      :prefix "s"
      :desc "consult-everything" "E" #'consult-everything)


;;
;;; Bureaucracy

(defun vlv-work-gen-payfit-report (on-site-days home-office-days &optional comment)
  "Copy the monthly Payfit expense comment to the clipboard.

ON-SITE-DAYS and HOME-OFFICE-DAYS are the number of days worked on site and
from home.  COMMENT is an optional string appended on a new line;
interactively, a prefix argument asks for it."
  (interactive
   (list (read-number "On site days: ")
         (read-number "Home Office days: ")
         (when current-prefix-arg
           (read-string "Comment: "))))
  (let* ((on-site-rate 15)
         (home-office-rate 10)
         (on-site-fee (* on-site-days on-site-rate))
         (home-office-fee (* home-office-days home-office-rate))
         (total (+ on-site-fee home-office-fee))
         (text (format "%d jours x %d€/jour sur site + %d jours x %d€/jour TT = %d€ + %d€ = %d€"
                       on-site-days on-site-rate home-office-days home-office-rate
                       on-site-fee home-office-fee total)))
    (unless (string-empty-p (or comment ""))
      (setq text (concat text "\n" comment)))
    ;; send the text to clipboard and log
    (kill-new text)
    (message "sent to clipboard: \"%s\"" text)))


;;
;;; Alarms

;; `alarm-clock-set' is autoloaded. Run once at startup, not on every
;; `doom/reload', which would stack duplicate alarms.  `run-at-time' fires an
;; "HH:MM" time that has already passed today immediately, so skip those.
;; `alarm-clock-set' also switches buffers behind our back, hence
;; `save-current-buffer'.
;;
;; The alarms are set again at every startup and `alarm-clock-restore' is never
;; called, so don't write them to the cache file on every `alarm-clock-set'.
(setq alarm-clock-auto-save nil)

(add-hook! 'doom-after-init-hook
  (let ((now (format-time-string "%H:%M")))
    (save-current-buffer
      (pcase-dolist (`(,time . ,message)
                     '(("08:30" . "🍵 Coffee time!")
                       ("10:00" . "🍵 Coffee time!")
                       ("12:00" . "nham nham!")
                       ("15:00" . "🍵 Coffee time!")
                       ("17:00" . "Go home!")))
        (when (string> time now)
          (alarm-clock-set time message))))))


;;
;;; Commit messages

;; Override commit-butler's "Conventional Commit" snippet to add the
;; "[VILAT] " prefix.  Prepended, so it shadows the package's copy.
(defvar vlv-work-snippets-dir (dir! "snippets/")
  "Directory holding the snippets shipped with this module.")

(when (modulep! :editor snippets)
  (after! yasnippet
    (add-to-list 'yas-snippet-dirs 'vlv-work-snippets-dir)
    ;; `yas-global-mode' has already loaded `yas-snippet-dirs' by now
    (when (bound-and-true-p yas-global-mode)
      (yas-load-directory vlv-work-snippets-dir t))))
