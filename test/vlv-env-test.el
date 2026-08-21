;;; test/vlv-env-test.el -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest vlv-env-work-p/true-for-work ()
  (let ((vlv-env "WORK"))
    (should (vlv-env-work-p))
    (should-not (vlv-env-personal-p))))

(ert-deftest vlv-env-work-p/false-for-personal ()
  (let ((vlv-env "PERSONAL"))
    (should-not (vlv-env-work-p))
    (should (vlv-env-personal-p))))
