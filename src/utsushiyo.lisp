(in-package :cl-user)
(defpackage utsushiyo
  (:use :cl)
  (:import-from :alexandria
                :read-file-into-string
		:write-string-into-file)
  (:import-from :split-sequence
		:split-sequence)
  (:import-from :cl-fad
		:file-exists-p
                :directory-exists-p
		:copy-file
	        :list-directory
	        :walk-directory
	        :delete-directory-and-files
		:directory-pathname-p
		:merge-pathnames-as-directory
                :merge-pathnames-as-file)
  (:export 
   :find-system-dir
   :exist-env-p
   :set-environment-directory
   :make-project-env
   :ensure-project-env
   :delete-project-env
   :get-attribute
   :set-attribute
   :ensure-attribute
   :bootstrap
   :init
   :get-help
   :get-config
   :get-env))
(in-package :utsushiyo)

#| 
Constant variables
|#
(defconstant +user-home-dirname+ (namestring (user-homedir-pathname)))

(defparameter *environment-directory* +user-home-dirname+)
(defun set-environment-directory (directory)
  (cond ((directory-pathname-p directory)
	 (setf *environment-directory* directory))
	((pathnamep directory)
	 (setf *environment-directory* (namestring directory)))
	(t
	 (error (format nil "The argument ~A is not directory" directory)))))


#|
Directory utilities
|#
(defun check-directory-duplicate-p (directory-pathname)
  "return t when target directory has been existed"
  (directory-exists-p directory-pathname))

(defun make-directory (new-directory-pathname)
  "make directory when target directory has not created yet"
  (unless (check-directory-duplicate-p new-directory-pathname)
    (ensure-directories-exist new-directory-pathname)))

(defun make-directories (&rest pathnames)
  "make multi-directories"
  (loop for path in pathnames
	unless (check-directory-duplicate-p path)
	  do (make-directory path)))

(defun decompose-pathname-string (pathname-string)
  (split-sequence "/" pathname-string :test #'string=))

(defun ensure-path-existence (pathname-string)
  (make-directories (decompose-pathname-string pathname-string)))

(defun find-system-dir (system-name-symbol)
  "retrieve target system directory which is based on quicklisp"
  (namestring (directory-namestring (asdf:system-relative-pathname system-name-symbol ""))))

(defun copy-directory (from to)
  "copy directory directory"
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

(defun make-project-env (project-name &key project-root-path utsushiyo-file-directory project-config-dir)
  "make prooject environment configuration class"
  (make-instance 'project-env
		 :project-env-name project-name
		 :project-root-path project-root-path
		 :utsushiyo-file-directory (if utsushiyo-file-directory
					       utsushiyo-file-directory
					       "src/utsushiyo/")
		 :config-dir (if project-config-dir
				 project-config-dir
				 (concatenate 'string
					      *environment-directory*
					      ".utsushiyo/"
					      project-name "/"))))

(defgeneric exist-env-p (env))
(defmethod exist-env-p ((env project-env))
  (directory-exists-p (config-dir env)))
(defmethod exist-env-p ((env string))
  (directory-exists-p
   (concatenate 'string
		*environment-directory*
		".utsushiyo/"
		env "/")))

(defgeneric ensure-project-env (project)
  (:method ((project project-env))
    (make-directory (config-dir project))
    (when (project-root-path project)
      (copy-directory
       (concatenate 'string
		    (project-root-path project)
		    (utsushiyo-file-directory project))
       (config-dir project)))))
    
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
    ;;(when (directory-exists-p (directory-namestring f-name))
      (write-string-into-file attribute-content f-name :if-exists :supersede :if-does-not-exist :create)))

(defmethod set-attribute ((project string) (attribute-name string) (attribute-content string))
  (let ((f-name (concatenate 'string
			     (config-dir (make-project-env project))
			     attribute-name)))
    (write-string-into-file attribute-content f-name :if-exists :supersede :if-does-not-exist :create)))

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
					    *environment-directory*
					    ".utsushiyo/"
					    project-name "/"))))

(defun bootstrap ()
  "ensure utsushiyo project oneselves' configulation"
  (ensure-project-env *utsushiyo-project*))

(defgetter get-help "help")
(defgetter get-config "config")
(defgetter get-env "env")

(defun init (project-env-name &key project-root-path
				(utsushiyo-file-directory "src/utsushiyo/")
				(project-config-dir (concatenate 'string
								 *environment-directory*
								 ".utsushiyo/"
								 project-env-name "/")))
  "ensure any project's configuration
usage example:
  (utsushiyo:init \"sample\" :project-root-path \"~/path/to/project/directory\"
                             :utsushiyo-file-directory \"config/\")
"
  (progn
    (bootstrap)
    (let ((project-env-instance
	    (make-project-env project-env-name
			      :project-root-path project-root-path
			      :utsushiyo-file-directory utsushiyo-file-directory
			      :project-config-dir project-config-dir)))
      (ensure-project-env project-env-instance))))

