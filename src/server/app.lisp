(in-package :cl-user)
(defpackage quickdocs.server.app
  (:use :cl)
  (:import-from :clack
                :call)
  (:export :error-log))
(in-package :quickdocs.server.app)

(cl-annot:enable-annot-syntax)

@export
(defclass <app> (ningle:<app>)
     ((error-log :type (or null pathname)
                 :initarg :error-log
                 :initform nil
                 :accessor error-log)))

(defmethod call ((this <app>) env)
  (if (slot-value this 'error-log)
      (let* (res
             (error-string (with-output-to-string (s)
                             (setf res
                                   (let ((*error-output* s))
                                     (call-next-method this env))))))
        (when (> (length error-string) 0)
          (with-open-file (out (slot-value this 'error-log)
                               :direction :output
                               :external-format :utf-8
                               :if-exists :append
                               :if-does-not-exist :create)
            (princ error-string out)))
        res)
      (call-next-method this env)))
