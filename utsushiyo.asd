(defsystem "utsushiyo"
  :version
  #.(read-file-string
     (subpathname *load-pathname* "src/utsushiyo-default/config/version"))
  :author "Tomoki ABURATANI"
  :license "MIT"
  :depends-on (:alexandria
	       :cl-fad
	       :split-sequence)
  :components ((:module "src"
                :components
                ((:file "utsushiyo")
		 (:module "utsushiyo-default"
		  :components
		  ((:module "config"
		    :components
			    ((:static-file "version")))
		   (:module "help"
		    :components
			    ((:static-file "bin-general-help")
			     (:static-file "bin-get-attribute-help")
			     (:static-file "bin-help-help")
			     (:static-file "bin-init-help")
			     (:static-file "bin-new-project-help")
			     (:static-file "bin-set-attribute-help")
			     (:static-file "template")))
		   (:module "env"
		    :components
			    ((:static-file "example"))))))))
  :description "Common Lisp Local Environment's values Management Framework"
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "utsushiyo-test"))))
