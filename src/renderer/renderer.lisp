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
  (:import-from :alexandria
                :when-let)
  (:import-from :clack.doc.readme
                :find-system-readme
                :readme->html)
  (:import-from :clack.doc.repository
                :project-url
                :repos-homepage))
(in-package :clack.doc.renderer)

(cl-annot:enable-annot-syntax)

@export
(defparameter *template-path*
              (asdf:system-relative-pathname :clack-doc "templates/"))

@export
(defun template-path (filename)
  (merge-pathnames filename *template-path*))

@export
(defun render-with-layout (&rest env &key title content &allow-other-keys)
  (declare (ignore title content))
  (let ((emb:*escape-type* :html))
    (emb:execute-emb
     (template-path "layout.tmpl")
     :env env)))

(defmethod render-documentation :around (ql-dist)
  (declare (ignore ql-dist))
  (apply #'render-with-layout (call-next-method)))

@export
(defmethod render-documentation ((this ql-dist:release))
  (ql-dist:ensure-installed this)
  (let ((project-name (slot-value this 'ql-dist:project-name))
        (systems (sort (ql-dist:provided-systems this)
                       #'(lambda (a b)
                           (string< (slot-value a 'ql-dist:name)
                                    (slot-value b 'ql-dist:name)))))
        authors maintainers licenses
        dependencies)
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
                          :test #'string=))
               (when (slot-boundp system 'asdf::load-dependencies)
                 (setf dependencies
                       (append dependencies (slot-value system 'asdf::load-dependencies)))))
    (setf dependencies
          (loop with dist = (ql-dist:dist "quicklisp")
                with results = nil
                for system-name in (remove-duplicates dependencies)
                for release = (slot-value (ql-dist:find-system-in-dist (string-downcase system-name) dist) 'ql-dist:release)
                for dependency-name = (slot-value release 'ql-dist:project-name)
                when (notany #'(lambda (system)
                                 (string-equal (slot-value system 'ql-dist:name)
                                               dependency-name))
                               systems)
                  do (pushnew dependency-name results)
                finally (return (reverse results))))
    (list
     :title project-name
     :content
     (emb:execute-emb (template-path "project.tmpl")
      :env (list
            :name project-name
            :ql-version (ql-dist:version
                         (slot-value this 'ql-dist:dist))
            :archive-url (slot-value this 'ql-dist::archive-url)
            :project-url (project-url project-name)
            :homepage (repos-homepage project-name)
            :readme (when-let (readme
                               (find-system-readme (asdf:find-system (slot-value (car systems) 'ql-dist:name))))
                      (readme->html
                       (car readme)))
            :dependencies (remove-duplicates dependencies :test #'eq)
            :authors (reverse authors)
            :maintainer (reverse maintainers)
            :licenses (reverse licenses))))))
