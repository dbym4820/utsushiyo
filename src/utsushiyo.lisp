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
					      "." project-name "/"))))

(defgeneric ensure-project-env (project))
(defmethod ensure-project-env ((project project-env))
  (make-directory (config-dir project)))

(defgeneric delete-project-env (project))
(defmethod delete-project-env ((project project-env))
  (delete-directory-and-files (config-dir project)))

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

;; (set-attribute :sample-project "user/user-name" "tomoki")
;; (set-attribute (make-project-env "sample-project" :project-config-dir "/home/user/.sample-project/") "user/user-name" "tomoki")
(defgeneric set-attribute (project attribute-name attribute-content))
(defmethod set-attribute ((project project-env) (attribute-name string) (attribute-content string))
  (let ((f-name (concatenate 'string (config-dir project) attribute-name)))
    (with-open-file (file-var f-name :direction :output :if-exists :overwrite :if-does-not-exist :create)
      (write-line attribute-content file-var))))

#|
Help utilities
|#
(defgeneric defhelp (command-name source-project target-project))
(defmethod defhelp ((command-name string) (source-project project-env) (target-project project-env))
  (copy-file
   (concatenate 'string (config-dir source-project) "src/helps/" command-name)
   (concatenate 'string (config-dir target-project) "helps/" command-name)
   :overwrite t))
(defmethod defhelp ((command-name string) (source-project project-env) (target-project string))
  (copy-file
   (concatenate 'string (project-root-path source-project) "src/helps/" command-name)
   (concatenate 'string target-project command-name)
   :overwrite t))

(defgeneric get-help (command project))
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
  (defhelp "example" *utsushiyo-project* (concatenate 'string +user-home-dirname+ ".utsushiyo/helps/")))


(defun project-config-bootstrap (project-env-name)
  (let ((project-env-instance (make-project-env project-env-name)))
    (ensure-project-env project-env-instance)
    (defhelp "example" project-env-instance (concatenate 'string +user-home-dirname+ ".utsushiyo/helps/"))))
  
