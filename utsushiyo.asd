(defsystem "utsushiyo"
  :version "0.1.0"
  :author "Tomoki ABURATANI"
  :license "MIT"
  :depends-on (:cl-ppcre
	       :uiop
	       :alexandria
	       :cl-fad
	       :local-time
	       :split-sequence)
  :serial t
  :components ((:module "src"
                :components
                ((:file "utsushiyo")
		 (:module "config"
		  :components
		  ((:static-file "example-attr")))
		 (:module "help"
		  :components
		  ((:static-file "example"))))))
  :description "Common Lisp Local Configuration Management Framework"
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "utsushiyo-test"))))
