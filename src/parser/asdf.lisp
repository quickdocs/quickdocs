(in-package :cl-user)
(defpackage quickdocs.parser.asdf
  (:use :cl
        :quickdocs.parser.class)
  (:import-from :trivial-backtrace
                :print-backtrace))
(in-package :quickdocs.parser.asdf)

(cl-annot:enable-annot-syntax)

(defvar *asdf-registered-system* nil)

(defun asdf-components (comp)
  (etypecase comp
    (asdf::source-file (list comp))
    (asdf::static-file nil)
    (asdf::component
     (loop for c in (slot-value comp 'asdf::components)
           append (asdf-components c)))))

(defun asdf-system-reload (system)
  #+quicklisp (ql:quickload (slot-value system 'asdf::name) :verbose nil)
  #-quicklisp (asdf:oos 'asdf:load-op system :verbose nil)
  (let ((macroexpand-hook *macroexpand-hook*))
    (setf *macroexpand-hook*
          (lambda (fun form env)
            (when (consp form)
              (case (first form)
                (cl:defpackage
                 (register-package-system
                  (princ-to-string (second form))
                  (slot-value system 'asdf::name)))
                ((cl:defun cl:defmacro)
                 (make-instance '<doc-function>
                    :name (second form)
                    :type (if (eq (first form) 'cl:defun)
                              :function
                              :macro)
                    :lambda-list (third form)))
                (cl:defgeneric
                 (make-instance '<doc-function>
                    :name (second form)
                    :type :generic
                    :lambda-list (third form)))
                (cl:defmethod
                 (let ((lambda-list-pos (position-if #'listp (cddr form))))
                   (make-instance '<doc-method>
                      :name (second form)
                      :qualifier (subseq (cddr form) 0 lambda-list-pos)
                      :lambda-list (nth lambda-list-pos (cddr form)))))
                (cl:defclass
                 (make-instance '<doc-class>
                    :name (second form)
                    :type :class))
                (cl:defstruct
                 (make-instance '<doc-class>
                    :name (let ((name-and-options (second form)))
                            (if (listp name-and-options)
                                (car name-and-options)
                                name-and-options))
                    :type :struct))
                (cl:defconstant
                 (apply #'make-instance '<doc-variable>
                        `(:name ,(second form)
                          :type :constant
                          ,@(if (= (length form) 2)
                                nil
                                (list :initial-value (third form))))))
                ((cl:defparameter cl:defvar)
                 (apply #'make-instance '<doc-variable>
                        `(:name ,(second form)
                          :type :variable
                          ,@(if (= (length form) 2)
                                nil
                                (list :initial-value (third form))))))
                (cl:deftype
                 (make-instance '<doc-type>
                    :name (second form)
                    :lambda-list (third form)))))
            (funcall macroexpand-hook fun form env)))
    (handler-bind ((warning 'muffle-warning))
      (loop with errors = nil
            for comp in (asdf-components system)
            do (handler-case (typecase comp
                               (asdf:cl-source-file
                                (asdf:perform (make-instance 'asdf:compile-op) comp)
                                (asdf:perform (make-instance 'asdf:load-op) comp))
                               (asdf:c-source-file
                                (asdf:perform (make-instance 'asdf:load-op) comp)))
                 (error (e)
                   (print-backtrace e :output *error-output*)
                   (push e errors)))
            finally
         (setf *macroexpand-hook* macroexpand-hook)
         (return (values (null errors) (nreverse errors)))))))

@export
(defun ensure-system-loaded (system &key force)
  (if (and (not force)
           (find system *asdf-registered-system* :test #'equal))
      (values t nil)
      (progn
        (asdf-system-reload system)
        (push system *asdf-registered-system*)
        (values t t))))
