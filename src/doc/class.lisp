#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.doc.class
  (:use :cl
        :split-sequence)
  (:import-from :clack.doc.util
                :external-symbol-p
                :lambda-list->specializers
                :map-tree)
  (:import-from :clack.doc.markdown
                :markdown-escape))
(in-package :clack.doc.class)

(cl-annot:enable-annot-syntax)

(defvar *doc-packages* nil)

(defclass <doc-base> ()
     ((type :initarg :type :initform nil :accessor doc-type)
      (name :initarg :name :accessor doc-name)))

@export
(defgeneric generate-documentation (doc)
  (:documentation "Implement this method and tell how to render the class."))

@export
(defclass <doc-package> (<doc-base>)
     ((systems :initform nil :accessor package-systems)
      (symbols :initform nil :accessor package-symbols)))

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
  (push this (package-symbols (find-package* (symbol-package* this)))))

@export
(defmethod externalp ((this <doc-symbol-base>))
  (external-symbol-p (doc-name this)))

@export
(defclass <doc-function> (<doc-symbol-base>)
     ((lambda-list :initarg :lambda-list :initform nil :accessor function-lambda-list)))

(defmethod initialize-instance :after ((this <doc-function>) &key)
  (unless (doc-type this)
    (setf (doc-type this) :function)))

(defmethod find-entity ((this <doc-function>))
  (symbol-function (doc-name this)))

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

(defmethod find-entity ((this <doc-method>))
  (or (find-method (symbol-function (doc-name this))
                   ;; FIXME: ugly
                   (and (method-qualifier this) (list (method-qualifier this)))
                   (lambda-list->specializers (function-lambda-list this)))
      (error "Method not found: ~A ~A"
             (doc-name this) (normalized-lambda-list this))))

@export
(defclass <doc-class> (<doc-symbol-base>)
     ((slots :initarg :slots :initform nil :accessor class-slots)
      (super-classes :initarg :superclasses :initform nil :accessor class-super-classes)))

(defmethod initialize-instance :after ((this <doc-class>) &key)
  (setf (doc-type this) :class))

(defmethod prepare ((this <doc-class>))
  (let ((class (find-entity this)))
    (setf (class-super-classes this)
          (loop for super in (c2mop:class-direct-superclasses class)
                unless (member (type-of super) '(built-in-class eql-specializer))
                  collect (class-name super)))
    (setf (class-slots this)
          (c2mop:class-direct-slots class))))

(defmethod find-entity ((this <doc-class>))
  (find-class (doc-name this)))

@export
(defclass <doc-variable> (<doc-symbol-base>) ())

(defmethod initialize-instance :after ((this <doc-variable>) &key)
  (setf (doc-type this) :variable))
