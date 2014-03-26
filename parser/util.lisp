(in-package :cl-user)
(defpackage quickdocs.parser.util
  (:use :cl)
  (:import-from :alexandria
                :with-gensyms))
(in-package :quickdocs.parser.util)

(cl-annot:enable-annot-syntax)

@export
(defun declared-special-p (symbol)
  "Returns true if SYMBOL is declared special."
  #+lispworks (sys:declared-special-p symbol)
  #+sbcl (eql :special (sb-int:info :variable :kind symbol))
  #+allegro (eq (sys:variable-information symbol) :special)
  #+clozure (ccl:proclaimed-special-p symbol))

@export
(defmethod external-symbol-p ((symb symbol) &optional pkg)
  (when (symbol-package symb)
    (let* (exported
           (exported (do-external-symbols (s (or pkg
                                                 (symbol-package symb))
                                             exported)
                       (push s exported))))
      (not (null (member symb exported :test #'eq))))))

@export
(defmethod external-symbol-p ((symb cons) &optional pkg)
  (if (eq (car symb) 'cl:setf)
      (external-symbol-p (cadr symb) pkg)
      (error 'simple-error ;; TODO: make a specific condition.
             :format-control "Invalid symbol: ~A"
             :format-arguments (list symb))))

(defun intern-eql-specializer* (object)
  #+ccl (ccl:intern-eql-specializer object)
  #+sbcl (sb-mop:intern-eql-specializer object)
  #+(or clisp ecl) (clos:intern-eql-specializer object)
  #+(or allegro abcl) (mop:intern-eql-specializer object)
  #+cmu (pcl:intern-eql-specializer object)
  #+lispworks `(eql ,object))

@export
(defun lambda-list->specializers (lambda-list)
  (loop for arg in lambda-list
        with args = nil
        if (listp arg)
          do (if (and (listp (cadr arg)) (eq (caadr arg) 'eql))
                 (push (intern-eql-specializer* (eval (cadadr arg))) args)
                 (push (find-class (cadr arg)) args))
        else if (find arg lambda-list-keywords)
               return (nreverse args)
        else do (push t args)
        finally (return (nreverse args))))

@export
(defun map-tree (f tree)
  (typecase tree
    (cons
     (cons (map-tree f (car tree))
           (if (cdr tree)
               (map-tree f (cdr tree))
               nil)))
    (t (funcall f tree))))

@export
(defun slot-value* (instance slot-name)
  (if (slot-boundp instance slot-name)
      (slot-value instance slot-name)
      nil))

@export
(defmacro with-ignoring-streams (streams &body body)
  (let ((null-stream (gensym "NULL-STREAM")))
    `(with-open-file (,null-stream
                      #P"/dev/null"
                      :direction :output
                      :if-exists :overwrite)
       (let (,@(loop for stream in streams
                     collect (list stream null-stream)))
         ,@body))))

@export
(defmacro with-ignoring-all-streams (&body body)
  `(with-ignoring-streams (*standard-output*
                           *error-output*
                           *debug-io*
                           *trace-output*)
     ,@body))

@export
(defmacro with-retrying (retry-count &body body)
  (with-gensyms (retry-recompile retry-continue retry-load-system e)
    `(let (,@(loop for symbol in (list retry-recompile
                                       retry-continue
                                       retry-load-system)
                   collect `(,symbol ,retry-count)))
       (handler-bind
           ((error (lambda (,e)
                     (if (and (> ,retry-recompile 0)
                              (find-restart 'asdf:try-recompiling))
                         (progn (decf ,retry-recompile) (invoke-restart 'asdf:try-recompiling))
                         (signal ,e))))
            (error (lambda (,e)
                     (if (and (> ,retry-continue  0) (find-restart 'continue))
                         (progn (decf ,retry-continue) (invoke-restart 'continue))
                         (signal ,e)))))
         ,@body))))
