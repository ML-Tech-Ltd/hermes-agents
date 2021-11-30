(defpackage hermes-agents/tests/main
  (:use :cl
        :hermes-agents
        :rove))
(in-package :hermes-agents/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :hermes-agents)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
           (ok (= 1 1))))
