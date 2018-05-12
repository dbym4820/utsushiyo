(defpackage utsushiyo-test
  (:use :cl
        :utsushiyo
        :prove))
(in-package :utsushiyo-test)

;; NOTE: To run this test file, execute `(asdf:test-system :utsushiyo)' in your Lisp.
(setf *enable-colors* nil)

(plan 1)

(subtest "project-env test"
	 (let ((test-instance
		 (make-instance 'project-env
				:project-env-name "utsushiyo"
				:project-root-path (namestring
						    (directory-namestring
						     (asdf:system-relative-pathname 'utsushiyo "")))
				:config-dir (concagtenate 'string utsushiyo::+user-home-dirname+ ".utsushiyo/"))))
	   (ok (bootstrap))
	   (is-type (get-help "example" (make-project-env "utsushiyo")) 'string)
	   (is-type (project-env-name test-instance) 'string)
	   (is-type (project-root-path test-instance) 'string)
	   (is-type (config-dir test-instance) 'string)))
	   
		 
(finalize)
