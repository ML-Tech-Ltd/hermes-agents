(defsystem "overmind-agents"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:lparallel
	       :random-state
               :datafly
	       :dbi
               :cl-mathstats
               :magicl
	       :cl-csv
               :computable-reals
               :sxql
	       :cl21
	       :uuid
	       :marshal
	       :salza2
	       :zlib
	       :flexi-streams
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
			((:file "main" :depends-on ("config" "db" "km"))
			 (:file "km")
			 (:file "config")
			 (:file "db")))
	       (:module "datasets"
			:components
			((:file "sunspot")
			 (:file "mg"))))
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
