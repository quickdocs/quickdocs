(in-package :cl-user)
(defpackage quickdocs.renderer.description
  (:use :cl)
  (:import-from :alexandria
                :when-let)
  (:import-from :ppcre
                :regex-replace
                :create-scanner)
  (:import-from :quickdocs.renderer.util
                :slurp-file))
(in-package :quickdocs.renderer.description)

(cl-annot:enable-annot-syntax)

(defvar *data-description-path*
    (asdf:system-relative-pathname :quickdocs "data/cliki/description.txt"))

@export
(defvar *description-db* (read-from-string (slurp-file *data-description-path*)))

(let ((prefix-scanner (ppcre:create-scanner "^cl-")))
  (defun find-main-system-in-release (release)
    (flet ((emit-prefix (name)
             (ppcre:regex-replace prefix-scanner name "")))
      (let ((project-name (emit-prefix (ql-dist:project-name release))))
        (find-if
         #'(lambda (s)
             (string= (emit-prefix (ql-dist:name s))
                      project-name))
         (ql-dist:provided-systems release))))))

@export
(defun project-description (project-name)
  (let* ((release (ql-dist:find-release project-name))
         (main-system (find-main-system-in-release release)))
    (or
     (when-let (main-system (and main-system
                                 (ignore-errors (asdf:find-system (ql-dist:name main-system)))))
       (when (slot-boundp main-system 'asdf::description)
         (let ((desc (slot-value main-system 'asdf::description)))
           (typecase desc
             (list (format nil "~{~A~^ ~}" desc))
             (string desc)
             (t (princ-to-string desc))))))
     (cdr (assoc project-name *description-db* :test #'string-equal)))))