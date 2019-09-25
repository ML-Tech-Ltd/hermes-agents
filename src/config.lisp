(defpackage overmind-agents.config
  (:use :cl
	:random-state)
  (:import-from :envy
                :config-env-var
                :defconfig)
  (:export :config
           :*application-root*
           :*static-directory*
           :*template-directory*
           :*plots-directory*
           :*data-directory*
	   :*rand-gen*
           :appenv
           :developmentp
           :productionp
	   :*db-path*))
(in-package :overmind-agents.config)

(defparameter *rand-gen* (make-generator :mersenne-twister-32))

(setf (config-env-var) "APP_ENV")

(defparameter *application-root*   (asdf:system-source-directory :neuropredictions))
(defparameter *static-directory*   (merge-pathnames #P"static/" *application-root*))
(defparameter *template-directory* (merge-pathnames #P"templates/" *application-root*))

;; for predict module
(defparameter *plots-directory*   (merge-pathnames #P"plots/" *application-root*))
(defparameter *data-directory*   (merge-pathnames #P"data/" *application-root*))

(defconfig :common
    `(:error-log #P"~/predictus-error.log"
      :databases ((:maindb :sqlite3 :database-name ":memory:"))))

(defconfig |development|
  '())

(defconfig |production|
  '())

(defconfig |test|
  '())

(defun config (&optional key)
  (envy:config #.(package-name *package*) key))

(defun appenv ()
  (uiop:getenv (config-env-var #.(package-name *package*))))

(defun developmentp ()
  (string= (appenv) "development"))

(defun productionp ()
  (string= (appenv) "production"))
