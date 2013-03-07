(in-package :cl-user)
(defpackage quickdocs.renderer
  (:use :cl)
  (:import-from :emb
                :execute-emb)
  (:import-from :alexandria
                :when-let
                :if-let
                :ensure-list)
  (:import-from :trivial-backtrace
                :print-backtrace)
  (:import-from :quickdocs.parser
                :parse-documentation)
  (:import-from :quickdocs.parser.util
                :with-retrying)
  (:import-from :quickdocs.quicklisp
                :ql-release-version)
  (:import-from :quickdocs.model.project
                :sorted-provided-systems
                :merged-slot-values
                :dependency-projects)
  (:import-from :quickdocs.renderer.readme
                :project-readme-in-html)
  (:import-from :quickdocs.renderer.repository
                :project-url
                :repos-homepage)
  (:import-from :quickdocs.renderer.category
                :project-categories)
  (:import-from :quickdocs.renderer.util
                :slurp-file))
(in-package :quickdocs.renderer)

(cl-annot:enable-annot-syntax)

@export
(defparameter *static-path*
              (asdf:system-relative-pathname :quickdocs "static/html/"))

@export
(defparameter *template-path*
              (asdf:system-relative-pathname :quickdocs "templates/"))

@export
(defparameter *data-path*
              (asdf:system-relative-pathname :quickdocs "data/"))

@export
(defparameter *layout-template* #P"layout.tmpl")

@export
(defun static-path (filename)
  (merge-pathnames filename *static-path*))

@export
(defun template-path (filename)
  (merge-pathnames filename *template-path*))

@export
(defun data-path (filename)
  (merge-pathnames filename *data-path*))

@export
(defun render-with-layout (&rest env &key title content &allow-other-keys)
  (declare (ignore title content))
  (let ((emb:*escape-type* :html))
    (emb:execute-emb
     (template-path *layout-template*)
     :env env)))

@export
(defmethod render-documentation ((this ql-dist:release))
  (ql-dist:ensure-installed this)
  (let ((project-name (slot-value this 'ql-dist:project-name)))
    (emb:execute-emb (template-path "project.tmpl")
     :env (list
           :name project-name
           :ql-version (ql-release-version this)
           :archive-url (slot-value this 'ql-dist::archive-url)
           :project-url (project-url project-name)
           :homepage (repos-homepage project-name)
           :readme (project-readme-in-html (car (sorted-provided-systems this)))
           :categories (project-categories project-name)
           :dependencies (mapcar #'ql-dist:project-name (dependency-projects this))
           :authors (merged-slot-values this 'asdf::author)
           :maintainer (merged-slot-values this 'asdf::maintainer)
           :licenses (merged-slot-values this 'asdf::licence)))))

(defun parse-cache (system)
  (let* ((release (ql-dist:release system))
         (path (data-path (format nil "parse/~A/~A"
                                  (ql-dist:project-name release)
                                  (ql-dist:name system)))))
    (when (fad:file-exists-p path)
      (read-from-string (slurp-file path)))))

@export
(defmethod render-api-reference ((this ql-dist:release) &key use-cache continue-on-error)
  (let ((project-name (slot-value this 'ql-dist:project-name))
        (systems (sorted-provided-systems this))
        errors)
    (emb:execute-emb (template-path "api.tmpl")
     :env `(:name ,project-name
            :system-list
            ,(remove-if #'null
              (mapcar #'(lambda (system)
                          (princ system) (fresh-line)
                          (if-let (cache (and use-cache
                                              (parse-cache system)))
                            cache
                            (handler-case (with-retrying 5 (parse-documentation system))
                              (error (e)
                                (if continue-on-error
                                    (progn
                                      (print-backtrace e :output *error-output*)
                                      (push (format nil "~A: ~A"
                                                    (slot-value system 'ql-dist:name)
                                                    e)
                                            errors)
                                      nil)
                                    (signal e))))))
               systems))
            :errors ,(nreverse errors)
            :archive-url ,(slot-value this 'ql-dist::archive-url)
            :project-url ,(project-url project-name)
            :homepage ,(repos-homepage project-name)))))
