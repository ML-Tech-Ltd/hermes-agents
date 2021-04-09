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

	       :rest-server

	       :overmind-common
	       :overmind-intuition
	       :overmind-input
	       :overmind-perception
	       )
  :components ((:module "vendor/fare-memoization-20180430-git"
			:components
			((:file "memoization")))
	       (:module "src"
			:components
			((:file "main" :depends-on ("config" "db" "km" "utils" "log" "api" "trading"))
			 (:file "trading" :depends-on ("log"))
			 (:file "km")
			 (:file "log")
			 (:file "api")
			 (:file "plotly-utils")
			 (:file "utils")
			 (:file "config" :depends-on ("utils"))
			 (:file "db" :depends-on ("trading"))))
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
               "rove"
	       "lparallel"
	       "random-state")
  :components ((:module "tests"
                :components
                ((:file "main" :depends-on ("config")))))
  :description "Test system for overmind-agents"

  :perform (test-op (op c) (symbol-call :rove :run c)))
