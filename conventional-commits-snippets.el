;; conventional-commit.el --- Conventional commit helper with transient menu
;;
;; This provides a YASnippet template and transient menu for creating
;; conventional commits with type, scope, ticket number, and descriptions.
;;
;; Installation:
;; 1. Save this file and load it: (load-file "path/to/conventional-commit.el")
;; 2. The snippet will be available in git-commit-mode
;; 3. Bind the transient: (define-key git-commit-mode-map (kbd "C-c c") 'conventional-commit-menu)
;;
;; Usage:
;; - In a git commit buffer, run M-x conventional-commit-menu or use the keybinding
;; - Or type "cc" and expand with yas-expand

(require 'yasnippet)
(require 'transient)

(defvar conventional-commit-type "feat"
  "The commit type selected from transient menu.")

(defvar conventional-commit-scope ""
  "The commit scope selected from transient menu.")

(defvar conventional-commit-ticket ""
  "The ticket/issue number selected from transient menu.")

(defvar conventional-commit-breaking nil
  "Whether this is a breaking change.")

(defun conventional-commit-set-type (type)
  "Set the commit TYPE."
  (setq conventional-commit-type type))

(defun conventional-commit-set-scope ()
  "Set the commit scope and return to menu."
  (interactive)
  (let ((scope (read-string "Scope (optional): " conventional-commit-scope)))
    (setq conventional-commit-scope scope)))

(defun conventional-commit-set-ticket ()
  "Set the ticket number and return to menu."
  (interactive)
  (let ((ticket (read-string "Ticket/Issue # (optional): " conventional-commit-ticket)))
    (setq conventional-commit-ticket ticket)))

(defun conventional-commit-toggle-breaking ()
  "Toggle breaking change flag."
  (interactive)
  (setq conventional-commit-breaking (not conventional-commit-breaking)))

(defun conventional-commit-insert ()
  "Insert the conventional commit template with selected values."
  (interactive)
  (let* ((scope-str (if (and conventional-commit-scope
                             (not (string-empty-p conventional-commit-scope)))
                        (format "(%s)" conventional-commit-scope)
                      ""))
         (breaking-str (if conventional-commit-breaking "!" ""))
         (ticket-str (if (and conventional-commit-ticket
                              (not (string-empty-p conventional-commit-ticket)))
                         (format " [%s]" conventional-commit-ticket)
                       ""))
         (prefix (format "%s%s%s:%s "
                         conventional-commit-type
                         scope-str
                         breaking-str
                         ticket-str)))
    ;; Close transient menu
    (transient-quit-one)
    ;; Insert the prefix and position cursor
    (insert prefix)
    (save-excursion
      (insert "\n\n\n\n"))))

(transient-define-prefix conventional-commit-menu ()
  "Transient menu for conventional commits."
  [:description
   (lambda ()
     (format "Preview: %s%s%s:%s [title]"
             conventional-commit-type
             (if (and conventional-commit-scope
                      (not (string-empty-p conventional-commit-scope)))
                 (format "(%s)" conventional-commit-scope)
               "")
             (if conventional-commit-breaking "!" "")
             (if (and conventional-commit-ticket
                      (not (string-empty-p conventional-commit-ticket)))
                 (format " [%s]" conventional-commit-ticket)
               "")))
   ["Commit Type"
    ("f" "feat: New feature" (lambda () (interactive) (conventional-commit-set-type "feat"))
     :transient t)
    ("x" "fix: Bug fix" (lambda () (interactive) (conventional-commit-set-type "fix"))
     :transient t)
    ("d" "docs: Documentation" (lambda () (interactive) (conventional-commit-set-type "docs"))
     :transient t)
    ("s" "style: Formatting" (lambda () (interactive) (conventional-commit-set-type "style"))
     :transient t)
    ("r" "refactor: Code refactoring" (lambda () (interactive) (conventional-commit-set-type "refactor"))
     :transient t)
    ("p" "perf: Performance" (lambda () (interactive) (conventional-commit-set-type "perf"))
     :transient t)
    ("t" "test: Tests" (lambda () (interactive) (conventional-commit-set-type "test"))
     :transient t)
    ("b" "build: Build system" (lambda () (interactive) (conventional-commit-set-type "build"))
     :transient t)
    ("c" "ci: CI/CD" (lambda () (interactive) (conventional-commit-set-type "ci"))
     :transient t)
    ("h" "chore: Maintenance" (lambda () (interactive) (conventional-commit-set-type "chore"))
     :transient t)
    ("v" "revert: Revert commit" (lambda () (interactive) (conventional-commit-set-type "revert"))
     :transient t)]
   ["Options"
    ("o" "Set scope" conventional-commit-set-scope
     :transient t
     :description (lambda ()
                    (if (and conventional-commit-scope
                             (not (string-empty-p conventional-commit-scope)))
                        (format "Scope: %s" conventional-commit-scope)
                      "Set scope")))
    ("n" "Set ticket #" conventional-commit-set-ticket
     :transient t
     :description (lambda ()
                    (if (and conventional-commit-ticket
                             (not (string-empty-p conventional-commit-ticket)))
                        (format "Ticket: %s" conventional-commit-ticket)
                      "Set ticket #")))
    ("!" "Toggle breaking change" conventional-commit-toggle-breaking
     :transient t
     :description (lambda ()
                    (format "Breaking: %s"
                            (if conventional-commit-breaking "YES" "NO"))))]
   ["Actions"
    ("RET" "Insert commit title" conventional-commit-insert)
    ("q" "Quit" transient-quit-one)]])

;; YASnippet definition
;; Save this to ~/.emacs.d/snippets/git-commit-mode/cc
;; # -*- mode: snippet -*-
;; # name: conventional-commit
;; # key: cc
;; # --
;; ${1:$$(yas-choose-value '("feat" "fix" "docs" "style" "refactor" "perf" "test" "build" "ci" "chore" "revert"))}${2:($3)}${4:!}: ${5:[${6:TICKET-123}] }${7:title}
;;
;; ${8:Short description}
;;
;; ${9:Long description}${10:
;;
;; BREAKING CHANGE: ${11:description}}

;; Alternative: programmatically add the snippet
;;;###autoload
(defun conventional-commit-setup-snippet ()
  "Setup the conventional commit snippet programmatically."
  (with-eval-after-load 'yasnippet
    (let ((snippet-dir (expand-file-name "snippets/git-commit-mode" user-emacs-directory)))
      (unless (file-exists-p snippet-dir)
        (make-directory snippet-dir t))
      (with-temp-file (expand-file-name "cc" snippet-dir)
        (insert "# -*- mode: snippet -*-
# name: conventional-commit
# key: cc
# --
${1:$$(yas-choose-value '(\"feat\" \"fix\" \"docs\" \"style\" \"refactor\" \"perf\" \"test\" \"build\" \"ci\" \"chore\" \"revert\"))}${2:($3)}${4:!}: ${5:[${6:TICKET-123}] }${7:title}

${8:Short description}

${9:Long description}${10:

BREAKING CHANGE: ${11:description}}")))))

;; Call this function to create the snippet file
;; (conventional-commit-setup-snippet)

(provide 'conventional-commit)
;;; conventional-commit.el ends here
