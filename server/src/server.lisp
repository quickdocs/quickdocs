(in-package :cl-user)
(defpackage quickdocs.server
  (:use :cl)
  (:import-from :quickdocs.server.config
                :config)
  (:import-from :clack
                :clackup)
  (:export :start
           :stop))
(in-package :quickdocs.server)

(defvar *appfile-path*
  (asdf:system-relative-pathname :quickdocs-server #P"app.lisp"))

(let (handler)
  (defun start (&rest args &key server port debug &allow-other-keys)
    (when handler
      (error "Server is already started."))
    (setf handler
          (apply #'clackup *appfile-path* args)))

  (defun stop ()
    (prog1
     (clack:stop handler)
     (setf handler nil))))
