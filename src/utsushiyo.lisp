(in-package :cl-user)
(defpackage utsushiyo
  (:use :cl)
  (:import-from :alexandria
                :read-file-into-string)
  (:import-from :uiop
                :directory-exists-p
		:file-exists-p
		:delete-file-if-exists)
  (:import-from :uiop/common-lisp
                :user-homedir-pathname)
  (:import-from :cl-fad
		:copy-file
	        :list-directory
	        :walk-directory
	        :delete-directory-and-files
		:directory-pathname-p
		:merge-pathnames-as-directory
                :merge-pathnames-as-file)
  (:import-from :cl-ppcre
		:split)
  (:export :project-env
           :project-env-name
	   :project-root-path
           :config-dir
           :make-project-env
	   :ensure-project-env
	   :delete-project-env
           :get-attribute
	   :set-attribute
	   :ensure-attribute
	   :get-help
	   :defhelp
	   :bootstrap
	   :project-config-bootstrap
	   :find-system-dir
	   :copy-file
	   :defgetter
	   :get-help
	   :get-config
	   :get-env))
(in-package :utsushiyo)

#| 
Constant valiables
|#
(defconstant +user-home-dirname+ (namestring (user-homedir-pathname)))

#|
Directory utilities
|#
(defun check-directory-duplicate-p (directory-pathname)
  (directory-exists-p directory-pathname))

(defun make-directory (new-directory-pathname)
  (unless (check-directory-duplicate-p new-directory-pathname)
    (ensure-directories-exist new-directory-pathname)))

(defun make-directories (&rest pathnames)
  (loop for path in pathnames
	unless (check-directory-duplicate-p path)
	  do (make-directory path)))

(defun find-system-dir (system-name-symbol)
  (eval `(namestring (directory-namestring (asdf:system-relative-pathname ',system-name-symbol "")))))

(defun copy-directory (from to)
  (flet ((rel-path (parent child)
           (subseq (namestring child)
                   (length (namestring parent)))))
    (walk-directory from
		    (lambda (child)
		      (if (directory-pathname-p child)
			  (ensure-directories-exist
			   (merge-pathnames-as-directory to (rel-path from child)))
			  (copy-file child
				     (merge-pathnames-as-file
				      to
				      (rel-path from child))
				     :overwrite t)))
		    :directories :breadth-first)))

#|
Project class
|#
(defclass project-env ()
  ((project-env-name :initarg :project-env-name :initform "sample-project" :accessor project-env-name)
   (project-root :initarg :project-root-path :accessor project-root-path)
   (utsushiyo-file-directory :initarg :utsushiyo-file-directory :accessor utsushiyo-file-directory)
   (project-config-dir :initarg :config-dir :accessor config-dir)))

(defun make-project-env (project-name &key project-config-dir)
  (make-instance 'project-env
		 :project-env-name project-name
		 :project-root-path (find-system-dir project-name)
		 :utsushiyo-file-directory "src/utsushiyo/"
		 :config-dir (if project-config-dir
				 project-config-dir
				 (concatenate 'string
					      +user-home-dirname+
					      ".utsushiyo/"
					      project-name "/"))))

(defgeneric ensure-project-env (project)
  (:method ((project project-env))
    (make-directory (config-dir project))
    (copy-directory
     (concatenate 'string
		  (project-root-path project)
		  (utsushiyo-file-directory project))
     (config-dir project))))
    
(defgeneric delete-project-env (project)
  (:method ((project project-env))
    (delete-directory-and-files (config-dir project))))

#|
Attribute utilities
|#
;; (get-attribute :sample-project "user/user-name") => "tomoki"
;; (get-attribute (make-project-env "sample-project" :project-config-dir "/home/user/.sample-project/") "user/user-name") => "tomoki"
(defgeneric get-attribute (project attribute-name))
(defmethod get-attribute ((project project-env) (attribute-name string))
  (let ((f-name (concatenate 'string
			     (config-dir project)
			     attribute-name)))
    (if (file-exists-p f-name)
	(read-file-into-string f-name)
	"")))
(defmethod get-attribute ((project string) (attribute-name string))
  (let ((f-name (concatenate 'string
			     (config-dir (make-project-env project))
			     attribute-name)))
    (if (file-exists-p f-name)
	(read-file-into-string f-name)
	"")))

(defgeneric set-attribute (project attribute-name attribute-content))
(defmethod set-attribute ((project project-env) (attribute-name string) (attribute-content string))
  (let ((f-name (concatenate 'string
			     (config-dir project)
			     attribute-name)))
    (with-open-file (file-var f-name :direction :output
				     :if-exists :overwrite
				     :if-does-not-exist :create)
      (write-line attribute-content file-var))))
(defmethod set-attribute ((project string) (attribute-name string) (attribute-content string))
  (let ((f-name (concatenate 'string
			     (config-dir (make-project-env project))
			     attribute-name)))
    (with-open-file (file-var f-name :direction :output
				     :if-exists :overwrite
				     :if-does-not-exist :create)
      (write-line attribute-content file-var))))

#|
Help utilities
|#
(defmacro defgetter (getter-name-symbol attribute-type-name)
  `(let* ((package *package*)
	  (*package* (find-package :utsushiyo)))
     (defgeneric ,getter-name-symbol (project command-name)
       (:method ((project string) (command-name string))
	 (get-attribute
	  (make-project-env project)
	  (concatenate 'string
		       ,attribute-type-name
		       "/"
		       command-name)))
       (:method ((project project-env) (command-name string))
	 (get-attribute
	  project
	  (concatenate 'string
		       ,attribute-type-name
		       "/"
		       command-name))))
     (setf *package* package)))

#|
Bootstrapping
|#
(defparameter *utsushiyo-project*
  (let ((project-name "utsushiyo"))
    (make-instance 'project-env
		   :project-env-name project-name
		   :project-root-path (find-system-dir project-name)
		   :utsushiyo-file-directory "src/utsushiyo-default/"
		   :config-dir (concatenate 'string
					    +user-home-dirname+
					    ".utsushiyo/"
					    project-name "/"))))


(defun bootstrap ()
  (ensure-project-env *utsushiyo-project*))

(defgetter get-help "help")
(defgetter get-config "config")
(defgetter get-env "env")

(defun project-config-bootstrap (project-env-name)
  (let ((project-env-instance (make-project-env project-env-name)))
    (ensure-project-env project-env-instance)))
