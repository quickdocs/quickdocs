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
  (:import-from :clack.doc.renderer
                :render-documentation))
(in-package :clack.doc.server)

(cl-annot:enable-annot-syntax)

(defvar *app* (make-instance '<app>))

(setf (route *app* "/project/:project-name")
      #'(lambda (params)
          (render-documentation
           (ql-dist:find-release (getf params :project-name)))))

(let (handler)
  @export
  (defun start-server ()
    (setf handler (clackup *app*)))

  @export
  (defun stop-server ()
    (stop handler)
    (setf handler nil)))
