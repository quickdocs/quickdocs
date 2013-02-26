(in-package :cl-user)
(defpackage quickdocs.renderer
  (:use :cl)
  (:import-from :emb
                :execute-emb)
  (:import-from :alexandria
                :when-let
                :ensure-list)
  (:import-from :trivial-backtrace
                :print-backtrace)
  (:import-from :quickdocs.quicklisp
                :ql-release-version)
  (:import-from :quickdocs.readme
                :find-system-readme
                :readme->html)
  (:import-from :quickdocs.repository
                :project-url
                :repos-homepage)
  (:import-from :quickdocs.parser
                :parse-documentation))
(in-package :quickdocs.renderer)

(cl-annot:enable-annot-syntax)

@export
(defparameter *static-path*
              (asdf:system-relative-pathname :quickdocs "static/html/"))

@export
(defparameter *template-path*
              (asdf:system-relative-pathname :quickdocs "templates/"))

@export
(defun static-path (filename)
  (merge-pathnames filename *static-path*))

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

(defun find-systems-in-release (release)
  (sort (ql-dist:provided-systems release)
        #'(lambda (a b)
            (string< (slot-value a 'ql-dist:name)
                     (slot-value b 'ql-dist:name)))))

(defmethod render-documentation :around (ql-dist)
  (declare (ignore ql-dist))
  (apply #'render-with-layout (call-next-method)))

@export
(defmethod render-documentation ((this ql-dist:release))
  (ql-dist:ensure-installed this)
  (let ((project-name (slot-value this 'ql-dist:project-name))
        (systems (find-systems-in-release this))
        authors maintainers licenses
        dependencies)
    (loop for ql-system in systems
          for system = (ignore-errors (asdf:find-system
                                       (slot-value ql-system 'ql-dist:name)))
          when system
            do (when (slot-boundp system 'asdf::author)
                 (setf authors
                       (append (ensure-list (slot-value system 'asdf::author))
                               authors)))
               (when (slot-boundp system 'asdf::maintainer)
                 (setf maintainers
                       (append (ensure-list (slot-value system 'asdf::maintainer))
                               maintainers)))
               (when (slot-boundp system 'asdf::licence)
                 (setf licenses
                       (append (ensure-list (slot-value system 'asdf::licence))
                               licenses)))
               (when (slot-boundp system 'asdf::load-dependencies)
                 (setf dependencies
                       (append dependencies (slot-value system 'asdf::load-dependencies)))))
    (setf authors (remove-duplicates authors :test #'string=))
    (setf maintainers (remove-duplicates maintainers :test #'string=))
    (setf licenses (remove-duplicates licenses :test #'string=))
    (setf dependencies
          (loop with dist = (ql-dist:dist "quicklisp")
                with results = nil
                for system-name in (remove-duplicates dependencies)
                for system-dist = (ql-dist:find-system-in-dist (string-downcase system-name) dist)
                for release = (and system-dist
                                   (slot-value system-dist 'ql-dist:release))
                for dependency-name = (and release
                                           (slot-value release 'ql-dist:project-name))
                when (and system-dist
                          (notany #'(lambda (system)
                                      (string-equal (slot-value system 'ql-dist:name)
                                                    dependency-name))
                                  systems))
                  do (pushnew dependency-name results)
                finally (return (reverse results))))
    (list
     :title (format nil "~A | Quickdocs" project-name)
     :content
     (emb:execute-emb (template-path "project.tmpl")
      :env (list
            :name project-name
            :ql-version (ql-release-version this)
            :archive-url (slot-value this 'ql-dist::archive-url)
            :project-url (project-url project-name)
            :homepage (repos-homepage project-name)
            :readme (when-let (readme
                               (find-system-readme (car systems)))
                      (readme->html
                       (car readme)))
            :dependencies (remove-duplicates dependencies :test #'eq)
            :authors (reverse authors)
            :maintainer (reverse maintainers)
            :licenses (reverse licenses))))))

@export
(defmethod render-api-reference :around (release)
  (declare (ignore release))
  (apply #'render-with-layout (call-next-method)))

@export
(defmethod render-api-reference ((this ql-dist:release))
  (let ((project-name (slot-value this 'ql-dist:project-name))
        (systems (find-systems-in-release this))
        errors)
    (list
     :title (format nil "~A | API Reference | Quickdocs" project-name)
     :content
     (emb:execute-emb (template-path "api.tmpl")
      :env `(:name ,project-name
             :system-list ,(remove-if #'null
                            (mapcar #'(lambda (system)
                                        (handler-case (parse-documentation system)
                                          (error (e)
                                            (print-backtrace e :output *error-output*)
                                            (push (format nil "~A: ~A"
                                                          (slot-value system 'ql-dist:name)
                                                          e) errors)
                                            nil))) systems))
             :errors ,(nreverse errors)
             :archive-url ,(slot-value this 'ql-dist::archive-url)
             :project-url ,(project-url project-name)
             :homepage ,(repos-homepage project-name))))))
