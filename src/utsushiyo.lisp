(defpackage utsushiyo
  (:use :cl)
  (:import-from :alexandria
                :read-file-into-string)
  (:import-from :uiop
                :directory-exists-p
		:delete-file-if-exists)
  (:import-from :uiop/common-lisp
                :user-homedir-pathname)
  (:import-from :cl-fad
		:copy-file
	        :list-directory
	        :walk-directory
	        :delete-directory-and-files)
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
	   :copy-file))
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

#|
Project class
|#
(defclass project-env ()
  ((project-env-name :initarg :project-env-name :initform "sample-project" :accessor project-env-name)
   (project-root :initarg :project-root-path :accessor project-root-path)
   (project-config-dir :initarg :config-dir :accessor config-dir)))

(defun make-project-env (project-name &key project-config-dir)
  (make-instance 'project-env
		 :project-env-name project-name
		 :project-root-path (find-system-dir project-name)
		 :config-dir (if project-config-dir
				 project-config-dir
				 (concatenate 'string
					      +user-home-dirname+
					      ".utsushiyo/"
					      project-name "/"))))

(defgeneric ensure-project-env (project)
  (:method ((project project-env))
    (make-directory (config-dir project))))

(defgeneric ensure-project-sub-directory (project directory-name)
  (:method ((project project-env) (directory-name string))
    (make-directory
     (format nil "~A~A/" (config-dir project) directory-name))))


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
  (let ((return-value nil)
	(f-name (concatenate 'string (config-dir project) attribute-name)))
    (with-open-file (file-var f-name :direction :input :if-does-not-exist :create)
      (setf return-value (read-line file-var)))
    (format nil "~A" return-value)))
(defmethod get-attribute ((project string) (attribute-name string))
  (let ((return-value nil)
	(f-name (concatenate 'string (config-dir (make-project-env project)) attribute-name)))
    (with-open-file (file-var f-name :direction :input :if-does-not-exist :create)
      (setf return-value (read-line file-var)))
    (format nil "~A" return-value)))


(defgeneric get-attribute-sentence (project attribute-name))
(defmethod get-attribute-sentence ((project project-env) (attribute-name string))
  (let ((return-string "")
	(f-name (concatenate 'string (config-dir project) attribute-name)))
    (with-open-file (file-var f-name :direction :input :if-exists nil :if-does-not-exist nil)
      (loop for line = (read-line file-var nil)
	    while line
	    do (setf return-string (format nil "~A~A~%" return-string line))))
    (format nil "~A" return-string)))
(defmethod get-attribute-sentence ((project string) (attribute-name string))
  (let ((return-string "")
	(f-name (concatenate 'string (config-dir (make-project-env project)) attribute-name)))
    (with-open-file (file-var f-name :direction :input :if-exists nil :if-does-not-exist nil)
      (loop for line = (read-line file-var nil)
	    while line
	    do (setf return-string (format nil "~A~A~%" return-string line))))
    (format nil "~A" return-string)))

;; (set-attribute "sample-project" "user/user-name" "tomoki")
;; (set-attribute (make-project-env "sample-project" :project-config-dir "/home/user/.sample-project/") "user/user-name" "tomoki")
(defgeneric set-attribute (project attribute-name attribute-content))
(defmethod set-attribute ((project project-env) (attribute-name string) (attribute-content string))
  (let ((f-name (concatenate 'string (config-dir project) attribute-name)))
    (with-open-file (file-var f-name :direction :output :if-exists :overwrite :if-does-not-exist :create)
      (write-line attribute-content file-var))))
(defmethod set-attribute ((project string) (attribute-name string) (attribute-content string))
  (let ((f-name (concatenate 'string (config-dir (make-project-env project)) attribute-name)))
    (with-open-file (file-var f-name :direction :output :if-exists :overwrite :if-does-not-exist :create)
      (write-line attribute-content file-var))))

#|
Help utilities
|#
(defgeneric defhelp (target-project-env command-name)
  (:method ((target-project-env project-env) (command-name string))
    (let ((base-file (concatenate 'string (project-root-path target-project-env) "src/helps/" command-name))
	  (new-file-name (concatenate 'string (config-dir target-project-env) "helps/" command-name)))
      (copy-file base-file new-file-name :overwrite t))))

(defgeneric get-help (command-name project))
(defmethod get-help ((help-command string) (project project-env))
  (get-attribute-sentence project (concatenate 'string "helps/" help-command)))
(defmethod get-help ((help-command string) (project string))
  (get-attribute-sentence (make-project-env project) (concatenate 'string "helps/" help-command)))

#|
Bootstrapping
|#
(defparameter *utsushiyo-project*
  (make-project-env "utsushiyo"))

(defun bootstrap ()
  (ensure-project-env *utsushiyo-project*)
  (progn
    (ensure-project-sub-directory *utsushiyo-project* "helps")
    (ensure-project-sub-directory *utsushiyo-project* "config")
    (defhelp *utsushiyo-project* "example")
    (defhelp *utsushiyo-project* "roswell-script")))

(defun project-config-bootstrap (project-env-name)
  (let ((project-env-instance (make-project-env project-env-name)))
    (ensure-project-env project-env-instance)
    (set-help project-env-instance "example")))
