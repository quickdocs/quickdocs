#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.doc.server
  (:use :cl)
  (:import-from :ningle
                :<app>
                :route)
  (:import-from :clack
                :clackup
                :stop)
  (:import-from :clack.builder
                :builder)
  (:import-from :clack.middleware.static
                :<clack-middleware-static>)
  (:import-from :clack.doc.renderer
                :render-documentation))
(in-package :clack.doc.server)

(cl-annot:enable-annot-syntax)

(defun build (app)
  (builder
   (<clack-middleware-static>
    :path (lambda (path)
            (when (ppcre:scan "^(?:/static/|/images/|/css/|/js/|/robot\\.txt$|/favicon.ico$)" path)
              (ppcre:regex-replace "^/static" path "")))
    :root (asdf:system-relative-pathname
           :clack-doc #p"static/"))
   app))

(defvar *app* (make-instance '<app>))

(setf (route *app* "/project/:project-name")
      #'(lambda (params)
          (render-documentation
           (ql-dist:find-release (getf params :project-name)))))

(let (handler)
  @export
  (defun start-server ()
    (setf handler (clackup (build *app*))))

  @export
  (defun stop-server ()
    (stop handler)
    (setf handler nil)))
