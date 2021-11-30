;; (ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)
(ql-dist:enable (ql-dist:find-dist "ultralisp"))
;; (ql:quickload :alexandria)
;; (ql:quickload :access)
;; (ql:dist-version "ultralisp")
;; (ql:quickload :ciel)
;; (asdf:asdf-version)
;; (ql:update-client)
;; (ql-dist:disable (ql-dist:find-dist "ultralisp"))
(defsystem "hermes-agents"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:ciel
               :clerk
	       :hu.dwim.def
	       :eazy-gnuplot
	       :uuid
	       :envy
	       :plotly-cl
	       :postmodern
	       :cl-cpus
	       :defenum
	       :fare-mop
	       :defenum
	       :genetic-algorithm

	       ;; logging
	       :dissect

               ;; hermes tech
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
			 (:file "trading" :depends-on ("log" "stat"))
			 (:file "stat")
			 (:file "km")
			 (:file "log")
			 (:file "plotly-utils")
			 (:file "utils")
			 (:file "config" :depends-on ("utils"))
			 (:file "db" :depends-on ("trading")))))
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
