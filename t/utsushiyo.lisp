(defpackage utsushiyo-test
  (:use :cl
        :utsushiyo
        :prove))
(in-package :utsushiyo-test)

;; NOTE: To run this test file, execute `(asdf:test-system :utsushiyo)' in your Lisp.
(setf *enable-colors* nil)

(plan 2)

(subtest "project-env test"
	 (ok (not (bootstrap))))

(subtest "environment create completition test"
	 (let ((test-instance
		 (make-instance 'utsushiyo::project-env
				:project-env-name "utsushiyo"
				:project-root-path (namestring
						    (directory-namestring
						     (asdf:system-relative-pathname 'utsushiyo "src/utsushiyo-default")))
				:config-dir (concatenate 'string utsushiyo::+user-home-dirname+ ".utsushiyo/"))))
	   (is-type (get-help (make-project-env "utsushiyo") "bin-general-help") 'string)
	   (is (utsushiyo::project-env-name test-instance) "utsushiyo")
	   (is-type (utsushiyo::project-root-path test-instance) 'string)
	   (is (utsushiyo::config-dir test-instance)
	       (concatenate 'string
			    utsushiyo::+user-home-dirname+
			    ".utsushiyo/"))))
		 
(finalize)
