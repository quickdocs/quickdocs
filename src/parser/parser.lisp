#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.doc.parser
  (:use :cl
        :clack.doc.class)
  (:import-from :alexandria
                :when-let)
  (:import-from :clack.doc.asdf
                :ensure-system-loaded)
  (:import-from :clack.doc.util
                :slot-value*))
(in-package :clack.doc.parser)

(cl-annot:enable-annot-syntax)

@export
(defmethod parse-documentation ((system asdf:system))
  (ensure-system-loaded system)
  `(:type :system
    :name ,(slot-value* system 'asdf::name)
    :author ,(slot-value* system 'asdf::author)
    :maintainer ,(slot-value* system 'asdf::maintainer)
    :version ,(slot-value* system 'asdf::version)
    :licence ,(slot-value* system 'asdf::licence)
    :description ,(slot-value* system 'asdf::description)
    :long-description ,(slot-value* system 'asdf::long-description)
    :depends-on ,(slot-value* system 'asdf::load-dependencies)
    :package-list
    ,(mapcar #'parse-documentation
      (reverse (find-system-packages system)))))

@export
(defmethod parse-documentation ((system ql-dist:system))
  (when-let (asdf-system (ignore-errors
                           (asdf:find-system (slot-value system 'ql-dist:name))))
    (parse-documentation asdf-system)))

@export
(defmethod parse-documentation ((this <doc-package>))
  `(:type :package
    :name ,(doc-name this)
    :documentation ,(documentation (find-entity this) t)
    :symbol-list
    ,(mapcar #'parse-documentation
      (reverse
       (remove-if-not
        #'externalp
        (package-symbols this))))))

@export
(defmethod parse-documentation ((this <doc-function>))
  `(:type ,(doc-type this)
    :name ,(doc-name this)
    :lambda-list ,(function-lambda-list this)
    :documentation ,(documentation (doc-name this) 'function)))

@export
(defmethod parse-documentation ((this <doc-method>))
  `(:type ,(doc-type this)
    :name ,(doc-name this)
    :lambda-list ,(normalized-lambda-list this)
    :documentation ,(documentation (find-entity this) t)))

@export
(defmethod parse-documentation ((this <doc-class>))
  (prepare this)
  `(:type ,(doc-type this)
    :name,(doc-name this)
    :super-class-list ,(class-super-classes this)
    :documentation ,(documentation (doc-name this) 'type)
    :slot-list ,(mapcar #'parse-documentation
                 (class-slots this))))

(defmethod parse-documentation ((this c2mop:standard-direct-slot-definition))
  (let* ((accessors (intersection (c2mop:slot-definition-readers this)
                                  (mapcar #'cadr (c2mop:slot-definition-writers this))))
         (readers (set-difference (c2mop:slot-definition-readers this)
                                  accessors))
         (writers (set-difference (mapcar #'cadr (c2mop:slot-definition-writers this))
                                  accessors)))
    `(:type :class-slot
      :name ,(c2mop:slot-definition-name this)
      :accessors ,accessors
      :readers ,readers
      :writers ,writers
      :documentation ,(documentation this t))))

@export
(defmethod parse-documentation ((this <doc-variable>))
  `(:type ,(doc-type this)
    :name ,(doc-name this)
    :documentation ,(documentation this 'variable)))
