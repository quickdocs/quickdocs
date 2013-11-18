(in-package :cl-user)
(defpackage quickdocs.parser.serialize
  (:use :cl)
  (:import-from :alexandria
                :compose)
  (:import-from :quickdocs.parser.util
                :external-symbol-p))
(in-package :quickdocs.parser.serialize)

(cl-annot:enable-annot-syntax)

@export
(defclass <serializable-symbol> ()
     ((name :type string
            :initarg :name
            :reader ssymbol-name)
      (package-name :type (or null string)
                    :initarg :package-name
                    :initform nil
                    :reader ssymbol-package-name)
      (externalp :type boolean
                 :initarg :externalp
                 :reader ssymbol-externalp)))

@export
(defmethod serialize ((this <serializable-symbol>))
  (list :name (ssymbol-name this)
        :package-name (ssymbol-package-name this)
        :externalp (ssymbol-externalp this)))

@export
(defun make-ssymbol (symbol)
  (make-instance '<serializable-symbol>
     :name (symbol-name symbol)
     :package-name (and (symbol-package symbol) (package-name (symbol-package symbol)))
     :externalp (external-symbol-p symbol)))

(defmethod print-object ((this <serializable-symbol>) stream)
  (format stream "~:[~A~:[::~;:~]~;~*~]~A"
          (string= #.(package-name (find-package :common-lisp))
                   (ssymbol-package-name this))
          (ssymbol-package-name this)
          (ssymbol-externalp this)
          (ssymbol-name this)))

@export
(defun serialize-symbol (symbol)
  (typecase symbol
    (symbol
     (funcall (compose #'serialize #'make-ssymbol) symbol))
    (t symbol)))
