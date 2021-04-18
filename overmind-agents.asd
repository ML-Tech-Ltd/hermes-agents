(defsystem "hermes-agents"
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

	       :hermes-common
	       :hermes-intuition
	       :hermes-input
	       :hermes-perception
	       )
  :components ((:module "vendor/fare-memoization-20180430-git"
			:components
			((:file "memoization")))
	       (:module "src"
			:components
			((:file "main" :depends-on ("config" "db" "km" "utils" "log" "trading"))
			 (:file "trading" :depends-on ("log"))
			 (:file "km")
			 (:file "log")
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
  :in-order-to ((test-op (test-op "hermes-agents/tests"))))

(defsystem "hermes-agents/tests"
  :author ""
  :license ""
  :depends-on ("hermes-agents"
               "rove"
	       "lparallel"
	       "random-state")
  :components ((:module "tests"
                :components
                ((:file "main" :depends-on ("config")))))
  :description "Test system for hermes-agents"

  :perform (test-op (op c) (symbol-call :rove :run c)))
