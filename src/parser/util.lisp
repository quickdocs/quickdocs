(in-package :cl-user)
(defpackage clack.doc.util
  (:use :cl))
(in-package :clack.doc.util)

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
  (let* (exported
         (exported (do-external-symbols (s (or pkg
                                               (symbol-package symb))
                                           exported)
                     (push s exported))))
    (not (null (member symb exported :test #'eq)))))

@export
(defmethod external-symbol-p ((symb cons) &optional pkg)
  (if (eq (car symb) 'cl:setf)
      (external-symbol-p (cadr symb) pkg)
      (error "Invalid symbol.")))

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
  (mapcar (lambda (e)
            (cond
              ((null e) nil)
              ((listp e) (map-tree f e))
              (t (funcall f e))))
          tree))

@export
(defun slot-value* (instance slot-name)
  (if (slot-boundp instance slot-name)
      (slot-value instance slot-name)
      nil))
