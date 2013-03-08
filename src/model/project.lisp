(in-package :cl-user)
(defpackage quickdocs.model.project
  (:use :cl)
  (:import-from :alexandria
                :when-let
                :ensure-list))
(in-package :quickdocs.model.project)

(cl-annot:enable-annot-syntax)

(defmethod sorted-provided-systems ((release ql-dist:release))
  (sort (copy-list (ql-dist:provided-systems release))
        #'(lambda (a b)
            (string< (ql-dist:name a)
                     (ql-dist:name b)))))

(defmethod sorted-provided-systems ((system ql-dist:system))
  (list system))

(defmethod sorted-provided-systems ((system asdf:system))
  (list
   (ql-dist:find-system-in-dist
    (slot-value system 'asdf::name)
    (ql-dist:dist "quicklisp"))))

@export
(defmethod merged-slot-values ((release ql-dist:release) slot-name)
  (loop for system in (sorted-provided-systems release)
        append (merged-slot-values system slot-name) into values
        finally (return (remove-duplicates values :test #'equal :from-end t))))

@export
(defmethod merged-slot-values ((system ql-dist:system) slot-name)
  (when-let (asdf-system (ignore-errors
                           (asdf:find-system
                            (slot-value system 'ql-dist:name))))
    (merged-slot-values asdf-system slot-name)))

@export
(defmethod merged-slot-values ((system asdf:system) slot-name)
  (flet ((emptyp (value)
           (typecase value
             (null t)
             (string (string= value ""))
             (t (null value)))))
    (when (and (slot-boundp system slot-name)
               (not (emptyp (slot-value system slot-name))))
      (ensure-list (slot-value system slot-name)))))

;;
;; Dependencies

(defun dependency-systems (component)
  (set-difference
   (loop with dist = (ql-dist:dist "quicklisp")
         for dependency in (merged-slot-values component 'asdf::load-dependencies)
         for system-name = (if (listp dependency)
                               (second dependency)
                               dependency)
         for system = (ql-dist:find-system-in-dist (string-downcase system-name) dist)
         when system
           collect system into systems
         finally (return (nreverse systems)))
   (sorted-provided-systems component)
   :key #'ql-dist:name
   :test #'string=))

@export
(defun dependency-projects (component)
  (remove-duplicates
   (mapcar #'ql-dist:release (dependency-systems component))
   :key #'ql-dist:project-name
   :test #'string-equal
   :from-end t))

@export
(defun depending-projects (project)
  (remove-if-not
   #'(lambda (release)
       (and (not (string= (ql-dist:name release)
                          (ql-dist:name project)))
            (find-if
             #'(lambda (dep)
                 (string= dep (ql-dist:project-name project)))
             ;; almost same as calling `dependency-projects`, but this would be faster because this don't think about the order.
             (mapcan #'ql-dist:required-systems
                     (ql-dist:provided-systems release)))))
   (ql-dist:provided-releases t)))
