(defsystem "overmind-agents"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:lparallel
	       :random-state
               :datafly
               :sxql
	       :cl21
	       :marshal
	       :envy
	       :cl-json
	       :dexador
	       :alexandria

	       :overmind-intuition
	       :overmind-input
	       :overmind-perception
	       )
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config"))
		 (:file "config"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "overmind-agents/tests"))))

(defsystem "overmind-agents/tests"
  :author ""
  :license ""
  :depends-on ("overmind-agents"
	       "overmind-code"
               "rove"
	       "lparallel"
	       "random-state")
  :components ((:module "tests"
                :components
                ((:file "main" :depends-on ("config")))))
  :description "Test system for overmind-agents"

  :perform (test-op (op c) (symbol-call :rove :run c)))
