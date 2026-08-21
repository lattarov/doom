;;; test/config-load-test.el -*- lexical-binding: t; -*-
;;
;; Smoke test: the synced Doom profile (modules from init.el + tangled
;; config.el) loads in a fresh, non-interactive Emacs without erroring.
;; Run after `doom sync`, with DOOMDIR pointed at this checkout — see
;; scripts/ci/run-unit-tests.sh.

(require 'ert)

(ert-deftest doom-profile/loads-without-error ()
  (should (fboundp 'doom-initialize))
  (doom-initialize nil nil)
  (should (stringp doom-version))
  (should (file-equal-p doom-user-dir default-directory)))
