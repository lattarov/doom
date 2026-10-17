#!/usr/bin/env bash
# check-literate-config-tangle.sh — Tangle config.org into a scratch directory and check that
# every resulting .el file (plus the hand-written ones) is readable Emacs Lisp:
# balanced parens, no invalid read syntax.
#
# config.el and packages.el are gitignored, so this checks that config.org
# produces them rather than diffing against a committed copy.
set -euo pipefail

REPO="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." &>/dev/null && pwd)"
EMACS="${EMACS:-emacs}"

out="$(mktemp -d)"
trap 'rm -rf "$out"' EXIT

cp "${REPO}/config.org" "$out/"
(cd "$out" && "$EMACS" --batch -Q -l org --eval '(org-babel-tangle-file "config.org")')

# Hand-written Lisp: init.el and the private modules under modules/.
mapfile -t handwritten < <(find "${REPO}/init.el" "${REPO}/modules" -name '*.el' | sort)

for f in config.el packages.el; do
    [[ -s "${out}/${f}" ]] || {
        echo "check-literate-config-tangle: config.org did not produce ${f}" >&2
        exit 1
    }
done

"$EMACS" --batch -Q --eval '
(let ((failed nil))
  (dolist (file command-line-args-left)
    (with-temp-buffer
      (insert-file-contents file)
      (emacs-lisp-mode)
      (goto-char (point-min))
      (let ((start (point-min)))
        (condition-case err
            (while (progn (forward-comment (buffer-size)) (not (eobp)))
              (setq start (point))
              (read (current-buffer)))
          (error
           (setq failed t)
           ;; Point the user at the form that failed to read, not at EOF.
           (message "%s:%d: %s" file (line-number-at-pos start)
                    (error-message-string err)))))))
  (setq command-line-args-left nil)
  (kill-emacs (if failed 1 0)))' \
    "${out}/config.el" "${out}/packages.el" \
    "${handwritten[@]}"

echo "check-literate-config-tangle: config.org tangles to readable config.el and packages.el"
