(in-package :cl-user)
(defpackage quickdocs.class
  (:use :cl)
  (:import-from :alexandria
                :when-let)
  (:import-from :quickdocs.util
                :external-symbol-p
                :lambda-list->specializers
                :map-tree)
  (:export :doc-name
           :doc-type
           :package-systems
           :package-symbols
           :class-slots
           :class-super-classes
           :function-lambda-list))
(in-package :quickdocs.class)

(cl-annot:enable-annot-syntax)

(defvar *doc-packages* nil)

(defclass <doc-base> ()
     ((type :initarg :type :initform nil :accessor doc-type)
      (name :initarg :name :accessor doc-name)))

(defmethod print-object ((doc <doc-base>) stream)
  (print-unreadable-object (doc stream :type t)
    (format stream "~(~A~)~:[~;~:* [~(~A~)]~]" (doc-name doc) (doc-type doc))))

@export
(defclass <doc-package> (<doc-base>)
     ((systems :initform nil :accessor package-systems)
      (symbols :initform nil :accessor package-symbols)))

@export
(defmethod find-entity ((this <doc-package>))
  (find-package (doc-name this)))

@export
(defun find-package* (package-name &key force)
  (let ((pkg (find-if #'(lambda (pkg) (string-equal package-name
                                                (doc-name pkg)))
                      *doc-packages*)))
    (if (or pkg (not force))
        pkg
        (let ((pkg (make-instance '<doc-package>
                      :name package-name)))
          (push pkg *doc-packages*)
          pkg))))

@export
(defun register-package-system (package-name system-name)
  (pushnew system-name
           (package-systems
            (find-package* package-name :force t))))

@export
(defun find-system-packages (system)
  (remove-if-not #'(lambda (pkg)
                     (find (slot-value system 'asdf::name)
                           (package-systems pkg)
                           :test #'string-equal))
                 *doc-packages*))

@export
(defclass <doc-symbol-base> (<doc-base>)
     ((docstring :initform nil :accessor docstring)
      (package :initarg :package :initform (princ-to-string (package-name *package*)) :accessor symbol-package*)))

(defmethod initialize-instance :after ((this <doc-symbol-base>) &key)
  ;; Ignoring this symbol when its package isn't contained in the current system.
  ;; As the result, some packages with the aim to inject or to override a package of other systems are ignored now.
  (when-let (pkg (find-package* (symbol-package* this)))
    (push this (package-symbols pkg))))

@export
(defmethod externalp ((this <doc-symbol-base>))
  (external-symbol-p (doc-name this)))

@export
(defclass <doc-function> (<doc-symbol-base>)
     ((lambda-list :initarg :lambda-list :initform nil :accessor function-lambda-list)))

(defmethod initialize-instance :after ((this <doc-function>) &key)
  (unless (doc-type this)
    (setf (doc-type this) :function)))

@export
(defmethod find-entity ((this <doc-function>))
  (symbol-function (doc-name this)))

@export
(defmethod normalized-lambda-list ((this <doc-function>))
  (map-tree #'(lambda (obj) (if (keywordp obj)
                            (format nil "~S" obj)
                            (format nil "~A" obj)))
            (function-lambda-list this)))

@export
(defclass <doc-method> (<doc-function>)
     ((qualifier :initarg :qualifier :initform nil :accessor method-qualifier)))

(defmethod initialize-instance :after ((this <doc-method>) &key)
  (setf (doc-type this) :method))

@export
(defmethod find-entity ((this <doc-method>))
  (or (find-method (eval `(function ,(doc-name this)))
                   (method-qualifier this)
                   (lambda-list->specializers (function-lambda-list this)))
      (error "Method not found: ~A ~A"
             (doc-name this) (normalized-lambda-list this))))

@export
(defclass <doc-class> (<doc-symbol-base>)
     ((slots :initarg :slots :initform nil :accessor class-slots)
      (super-classes :initarg :superclasses :initform nil :accessor class-super-classes)))

(defmethod initialize-instance :after ((this <doc-class>) &key)
  (unless (slot-boundp this 'type)
    (setf (doc-type this) :class)))

@export
(defmethod prepare ((this <doc-class>))
  (let ((class (find-entity this)))
    (setf (class-super-classes this)
          (loop for super in (c2mop:class-direct-superclasses class)
                unless (or (eq (class-name super) 'standard-object)
                           (member (type-of super) '(built-in-class eql-specializer)))
                  collect (class-name super)))
    (setf (class-slots this)
          (c2mop:class-direct-slots class))))

@export
(defmethod find-entity ((this <doc-class>))
  (find-class (doc-name this)))

@export
(defclass <doc-variable> (<doc-symbol-base>) ())

(defmethod initialize-instance :after ((this <doc-variable>) &key)
  (unless (doc-type this)
    (setf (doc-type this) :variable)))

@export
(defclass <doc-type> (<doc-function>) ())

(defmethod initialize-instance :after ((this <doc-type>) &key)
  (setf (doc-type this) :type))
