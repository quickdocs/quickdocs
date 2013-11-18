(in-package :cl-user)
(defpackage quickdocs.model.system
  (:use :cl
        :annot.class
        :caveman2.db)
  (:import-from :quickdocs.server.config
                :*data-directory*)
  (:import-from :quickdocs.util
                :slurp-file)
  (:import-from :ppcre
                :regex-replace))
(in-package :quickdocs.model.system)

(cl-annot:enable-annot-syntax)

@export
@export-accessors
(defclass <system> ()
  ((id :type integer
       :initarg :|id|
       :reader system-id)
   (project-id :type integer
               :initarg :|project_id|
               :reader system-project-id)
   (name :type string
         :initarg :|name|
         :reader system-name)
   (version :type (or null string)
            :initarg :|version|
            :reader system-version)
   (description :type (or null string)
                :initarg :|description|
                :reader system-description)
   (long-description :type (or null string)
                     :initarg :|long_description|
                     :reader system-long-description)
   (license :type (or null string)
            :initarg :|license|
            :reader system-license)))

(defmethod initialize-instance :around ((system <system>) &rest initargs)
  (loop for column in '(:|description| :|long_description| :|license|)
        when (getf initargs column)
          do (setf (getf initargs column)
                   (babel:octets-to-string (getf initargs column))))
  (apply #'call-next-method system initargs))

@export
(defun search-systems-by-project-id (project-id)
  (let ((systems-row (select-all (connect-db) :*
                       (from :system)
                       (where (:= :system.project_id project-id)))))
    (mapcar (lambda (row) (apply #'make-instance '<system> row))
            systems-row)))

@export
(defun find-primary-system (project-id project-name)
  (select-one (connect-db) :*
    (from :system)
    (where (:and (:= :system.project_id project-id)
                 (:in :name (list project-name
                                  (ppcre:regex-replace "^cl-" project-name "")))))
    (limit 1)))

@export
(defgeneric system-parsed-data (system dist-version))

@export
(defmethod system-parsed-data ((system <system>) dist-version)
  (let* ((file (merge-pathnames (format nil "~A/parse/~A"
                                        dist-version
                                        (system-name system))
                                *data-directory*))
         (content (slurp-file file)))
    (and content
         (read-from-string content))))

@export
(defgeneric system-parse-error (system dist-version))

@export
(defmethod system-parse-error ((system <system>) dist-version)
  (let ((file (merge-pathnames (format nil "~A/error/~A"
                                       dist-version
                                       (system-name system))
                               *data-directory*)))
    (slurp-file file)))
