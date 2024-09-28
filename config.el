;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dark+)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; emacs general configs
;; Save what you enter into minibuffer prompts
(setq history-length 25)
(savehist-mode 1)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)
;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; text editing
;; Remember and restore the last cursor location of opened files
(save-place-mode 1)
;;
;; Org Roam related configuration
(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package! conda
  :config
  (setq conda-anaconda-home (expand-file-name "~/miniconda3"))
  (setq conda-env-home-directory (expand-file-name "~/miniconda3"))
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell))

;; treemacs
(after! treemacs
  (map! :leader
        (:prefix ("t" . "toggle")
         :desc "Toggle treemacs" "t" #'treemacs))
  )

;; vterm
(after! vterm
  (map! :leader
        (:prefix ("t" . "toggle")
         :desc "Toggle vterm" "v" #'+vterm/toggle))
  )

;; dirvish
(map! :leader
      (:prefix ("t" . "toggle")
       :desc "Toggle dirvish" "d" #'dirvish-side))

(setq dirvish-side-width 80)

;; YASNIPPET
(defun yasnippet-extract-arg-and-type (arg)
  "Extract argument name and type from a string of the form 'arg: type'."
  (let* ((parts (split-string arg ": ")))
    (if (= (length parts) 2)
        (cons (car parts) (cadr parts))
      (cons (car parts) "type"))))

(defun yasnippet-python-params-doc (arg-string)
  "Generates a docstring for parameters in NumPy style with optional type annotations.
If there are no arguments, returns ''."
  (let* ((args (split-string arg-string ", "))
         (formatted-doc
          (if (string-blank-p arg-string)
              ""
            (mapconcat (lambda (arg)
                         (let* ((arg-pair (yasnippet-extract-arg-and-type arg))
                                (arg-name (car arg-pair))
                                (arg-type (cdr arg-pair)))
                           ;; TODO: Generate placeholders for each parameter description.
                           (format "%s : %s\n        TODO: Description of %s." arg-name arg-type arg-name)))
                       args
                       "\n    "))))
    formatted-doc))

(defun yasnippet-python-parameter-assignments (arg-string)
  "Convert a comma-separated string of arguments into self assignments.
If there are no arguments, returns 'pass'."
  (let* ((args (split-string arg-string ", "))
         (assignments
          (if (string-blank-p arg-string)
              "pass"
            (mapconcat (lambda (arg)
                         (let* ((parts (split-string arg ": "))
                                (name (car parts)))
                           (format "self.%s = %s" name name)))
                       args
                       "\n        "))))
    assignments))
