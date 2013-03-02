(in-package :cl-user)
(defpackage quickdocs.quicklisp
  (:use :cl)
  (:import-from :alexandria
                :when-let))
(in-package :quickdocs.quicklisp)

(cl-annot:enable-annot-syntax)

@export
(defmethod ql-release-version ((release ql-dist:release))
  (when-let (match
                (nth-value 1
                           (ppcre:scan-to-strings "beta\\.quicklisp\\.org/archive/[^/]+/([^/]+)" (slot-value release 'ql-dist::archive-url))))
    (aref match 0)))

@export
(defun find-systems-in-release (release)
  (sort (ql-dist:provided-systems release)
        #'(lambda (a b)
            (string< (slot-value a 'ql-dist:name)
                     (slot-value b 'ql-dist:name)))))
