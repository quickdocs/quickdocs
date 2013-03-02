(in-package :cl-user)
(defpackage quickdocs.renderer.category
  (:use :cl)
  (:import-from :quickdocs.renderer.util
                :slurp-file))
(in-package :quickdocs.renderer.category)

(cl-annot:enable-annot-syntax)

(defvar *data-categories-path*
    (asdf:system-relative-pathname :quickdocs "data/cliki/categories.txt"))

@export
(defvar *category-db* (read-from-string (slurp-file *data-categories-path*)))

@export
(defun project-categories (project-name)
  (cdr (assoc project-name *category-db* :test #'string-equal)))