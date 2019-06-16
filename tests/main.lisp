(defpackage overmind-agents/tests/main
  (:use :cl
        :overmind-agents
        :rove))
(in-package :overmind-agents/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :overmind-agents)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
