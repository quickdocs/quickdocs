(in-package :cl-user)
(defpackage quickdocs.updater.model.system
  (:use :cl)
  (:import-from :quickdocs.updater.util
                :run-in-process)
  (:import-from :quickdocs.parser.util
                :with-retrying)
  (:import-from :alexandria
                :ensure-list))
(in-package :quickdocs.updater.model.system)

(cl-annot:enable-annot-syntax)

@export
(defun system-info (system-name)
  (labels ((safety-princ-to-string (obj)
             (etypecase obj
               (null nil)
               (list (format nil "~{~A~^~%~}"
                             (ensure-list obj)))
               (string obj)))
           (emit-quote (obj)
             (if (and (listp obj)
                      (eq 'cl:quote (car obj)))
                 (emit-quote (cadr obj))
                 obj)))
    (let* ((*standard-output* (make-broadcast-stream))
           (*error-output* (make-broadcast-stream))
           (system (with-retrying 3 (asdf:find-system system-name nil))))
      (when system
        (list
         :author (emit-quote (asdf:system-author system))
         :maintainer (emit-quote (asdf:system-maintainer system))
         :version (slot-value system 'asdf:version)
         :description (safety-princ-to-string (asdf:system-description system))
         :long-description (safety-princ-to-string (asdf:system-long-description system))
         :homepage (asdf:system-homepage system)
         :license (asdf:system-license system))))))

@export
(defun system-info-in-process (system-name)
  (let ((res (with-output-to-string (s)
               (run-in-process `(system-info ,system-name)
                               :output s))))
    (when res
      (read-from-string res))))

