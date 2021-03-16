(defsystem "overmind-agents"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:lparallel
               ;; :datafly
	       ;; :dbi
               :cl-mathstats
	       :clerk
               ;; :magicl
	       :cl-csv
               :computable-reals
	       :cl-mathstats
               ;; :sxql
	       :cl21
	       :uuid
	       :salza2
	       :zlib
	       :flexi-streams
	       :envy
	       :cl-json
	       :dexador
	       :alexandria
	       :sigma
	       :plotly-cl
	       :postmodern
	       :cl-cpus
	       :bordeaux-threads
	       :defenum
	       :fare-mop
	       :fare-memoization

	       ;; logging
	       :dissect

	       :swank

	       :overmind-intuition
	       :overmind-input
	       :overmind-perception
	       )
  :components ((:module "vendor/fare-memoization-20180430-git"
			:components
			((:file "memoization")))
	       (:module "src"
			:components
			((:file "main" :depends-on ("config" "db" "km" "utilities"))
			 (:file "km")
			 (:file "plotly-utils")
			 (:file "utilities")
			 (:file "config" :depends-on ("utilities"))
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
