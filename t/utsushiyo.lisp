(defpackage utsushiyo-test
  (:use :cl :prove)
  (:import-from :utsushiyo
		:bootstrap :init
		:set-attribute :get-attribute))
(in-package :utsushiyo-test)

;; NOTE: To run this test file, execute `(asdf:test-system :utsushiyo)' in your Lisp.
(setf *enable-colors* nil)

(plan 4)

(subtest "project-env test"
  (ok (not (bootstrap))))

(subtest "make 'sample' project environment test"
  (pass (init "sample-project")))

(subtest "set-attribute test"
  (pass (set-attribute "sample-project" "sample-attribute" "日本語もいける")))

(subtest "get-attribute test"
  (is (get-attribute "sample-project" "sample-attribute") "日本語もいける"))
		 
(finalize)
