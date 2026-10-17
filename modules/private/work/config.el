;;; private/work/config.el -*- lexical-binding: t; -*-

(defgroup vlv-work nil
  "Work-only configuration."
  :group 'convenience)


;;
;;; External tools on native Windows

(let* ((emacs-install-path "c:/Emacs/")
       (runtimes-path      (expand-file-name "runtimes/" emacs-install-path))
       (cygwin-path        "C:/cygwin64")
       (paths-to-add (list emacs-install-path
                           runtimes-path
                           (expand-file-name "fd" runtimes-path)
                           (expand-file-name "rg" runtimes-path)
                           cygwin-path
                           (expand-file-name "bin" cygwin-path))))
  ;; Add each existing path to `exec-path' and to the PATH env var
  (dolist (path paths-to-add)
    (let ((dir (directory-file-name (expand-file-name path))))
      (when (file-directory-p dir)
        ;; Add to Emacs' exec-path (used by call-process, etc.)
        (add-to-list 'exec-path dir)
        ;; Also add to the OS PATH environment so external tools can be found
        (setenv "PATH" (concat dir ";" (getenv "PATH")))))))


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
  (setq tightvnc-machines
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
;;; Visual Studio

(defun vlv-find-and-open-sln-in-visual-studio ()
  "Find .sln files in the current Projectile project and open one in Visual Studio."
  (interactive)
  (let* ((project-root (projectile-project-root))
         (sln-files (directory-files-recursively project-root "\\.sln$")))
    (if (null sln-files)
        (error "No .sln files found in the project."))
    (let ((chosen-sln (if (= (length sln-files) 1)
                          (car sln-files)
                        (completing-read "Choose a solution: " sln-files nil t))))
      (let* ((command (format "start devenv \"%s\"" chosen-sln)))
        (message "Running: %s" command)
        (start-process-shell-command "open-sln" nil command)))))

(map! :leader
      :prefix "o"
      :desc "Find project SLN and open Visual Studio" "s" #'vlv-find-and-open-sln-in-visual-studio)


;;
;;; Wind River Workbench

(defcustom vlv--workbench3-path
  "/mnt/c/WindRiver_6947/workbench-3.3/wrwb/platform/x86-win32/eclipse/wrwb-x86-win32.exe"
  "The path to the executable or script to start WindRiver 3 workbench IDE."
  :type 'file
  :group 'vlv-work)

(defcustom vlv--workbench4-path
  "/mnt/c/089-VXWORKS_CALIA4/workbench-4/startWorkbench.bat"
  "The path to the executable or script to start WindRiver 4 workbench IDE."
  :type 'file
  :group 'vlv-work)

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
      :desc "WindRiver Workbench 3" "W" #'vlv-open-windriver3-workbench
      :desc "WindRiver Workbench 4" "w" #'vlv-open-windriver4-workbench)


;;
;;; consult-everything (Windows search)

(after! consult-everything
  ;; the .exe makes it work on wsl
  (setopt consult-everything-args "es.exe -r"))

(map! :leader
      :prefix "s"
      :desc "consult-everything" "E" #'consult-everything)


;;
;;; Tree-sitter

;; See README.org for where the compiled grammars live on Windows.
(after! treesit
  (add-to-list 'treesit-language-source-alist
               '(csharp "https://github.com/tree-sitter/tree-sitter-c-sharp.git")))


;;
;;; Bureaucracy

(defun vlv-work-gen-payfit-report (on-site-days home-office-days &optional comment)
  "Copy the monthly Payfit expense comment to the clipboard.

ON-SITE-DAYS and HOME-OFFICE-DAYS are the number of days worked on site and
from home.  COMMENT is an optional string appended on a new line."
  (interactive "nOn site days: \nnHome Office days: ")
  (let* ((on-site-fee (* on-site-days 15))
         (home-office-fee (* home-office-days 10))
         (total (+ on-site-fee home-office-fee))
         (comment (if comment (format " \n %s" comment) ""))
         (text (format "%d jours x 15€/jour sur site + %d jours x 10€/jour TT = %d€ + %d€ = %d%s€"
                       on-site-days home-office-days on-site-fee home-office-fee total comment)))
    ;; send the text to clipboard and log
    (kill-new text)
    (message "sent to clipboard: \"%s\"" text)))


;;
;;; Alarms

;; `alarm-clock-set' is autoloaded. Run once at startup, not on every
;; `doom/reload', which would stack duplicate alarms.
(add-hook! 'doom-after-init-hook
  (alarm-clock-set "08:30" "🍵 Coffee time!")
  (alarm-clock-set "10:00" "🍵 Coffee time!")
  (alarm-clock-set "12:00" "nham nham!")
  (alarm-clock-set "15:00" "🍵 Coffee time!")
  (alarm-clock-set "17:00" "Go home!"))


;;
;;; Commit messages

(after! commit-butler
  (setq commit-butler-prefix "[VILAT] "))
