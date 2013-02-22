#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.doc.renderer
  (:use :cl)
  (:import-from :emb
                :execute-emb)
  (:import-from :clack.doc.readme
                :find-system-readme
                :readme->html))
(in-package :clack.doc.renderer)

(cl-annot:enable-annot-syntax)

(defparameter *template-path*
              (asdf:system-relative-pathname :clack-doc "templates/"))

(defun template-path (filename)
  (merge-pathnames filename *template-path*))

(defmethod render-documentation :around (ql-dist)
  (declare (ignore ql-dist))
  (let ((emb:*escape-type* :html))
    (emb:execute-emb
     (template-path "layout.tmpl")
     :env (call-next-method))))

@export
(defmethod render-documentation ((this ql-dist:release))
  (let ((project-name (slot-value this 'ql-dist:project-name))
        (systems (ql-dist:provided-systems this))
        authors maintainers licenses)
    (loop for ql-system in systems
          for system = (ignore-errors (asdf:find-system
                                       (slot-value ql-system 'ql-dist:name)))
          when system
            do (when (slot-boundp system 'asdf::author)
                 (pushnew (slot-value system 'asdf::author)
                          authors
                          :test #'string=))
               (when (slot-boundp system 'asdf::maintainer)
                 (pushnew (slot-value system 'asdf::maintainer)
                          maintainers
                          :test #'string=))
               (when (slot-boundp system 'asdf::licence)
                 (pushnew (slot-value system 'asdf::licence)
                          licenses
                          :test #'string=)))
    (list
     :title project-name
     :content
     (emb:execute-emb (template-path "project.tmpl")
      :env (list
            :name project-name
            :ql-version (ql-dist:version
                         (slot-value this 'ql-dist:dist))
            :readme (readme->html
                     (car (find-system-readme (asdf:find-system (slot-value (car systems) 'ql-dist:name)))))
            :authors authors
            :maintainer maintainers
            :licenses licenses)))))
