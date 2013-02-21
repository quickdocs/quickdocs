#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.doc.generator
  (:use :cl
        :clack.doc.class)
  (:import-from :clack.doc.asdf
                :ensure-system-loaded)
  (:import-from :clack.doc.util
                :slot-value*))
(in-package :clack.doc.generator)

(cl-annot:enable-annot-syntax)

@export
(defmethod generate-documentation ((system asdf:system))
  (ensure-system-loaded system)
  `(:system ,(slot-value* system 'asdf::name)
    :author ,(slot-value* system 'asdf::author)
    :maintainer ,(slot-value* system 'asdf::maintainer)
    :version ,(slot-value* system 'asdf::version)
    :licence ,(slot-value* system 'asdf::licence)
    :description ,(slot-value* system 'asdf::description)
    :long-description ,(slot-value* system 'asdf::long-description)
    :depends-on ,(slot-value* system 'asdf::load-dependencies)
    :package-list
    ,(mapcar #'generate-documentation
      (reverse (find-system-packages system)))))

@export
(defmethod generate-documentation ((this <doc-package>))
  `(:package ,(doc-name this)
    :documentation ,(documentation (find-entity this) t)
    :symbol-list
    ,(mapcar #'generate-documentation
      (reverse
       (remove-if-not
        #'externalp
        (package-symbols this))))))

@export
(defmethod generate-documentation ((this <doc-function>))
  `(,(doc-type this) ,(doc-name this)
    :lambda-list ,(function-lambda-list this)
    :documentation ,(documentation (doc-name this) 'function)))

@export
(defmethod generate-documentation ((this <doc-method>))
  `(,(doc-type this) ,(doc-name this)
    :lambda-list ,(normalized-lambda-list this)
    :documentation ,(documentation (find-entity this) t)))

@export
(defmethod generate-documentation ((this <doc-class>))
  (prepare this)
  `(,(doc-type this) ,(doc-name this)
    :super-class-list ,(class-super-classes this)
    :documentation ,(documentation (doc-name this) 'type)
    :slot-list ,(mapcar #'generate-documentation
                 (class-slots this))))

(defmethod generate-documentation ((this c2mop:standard-direct-slot-definition))
  (let* ((accessors (intersection (c2mop:slot-definition-readers this)
                                  (mapcar #'cadr (c2mop:slot-definition-writers this))))
         (readers (set-difference (c2mop:slot-definition-readers this)
                                  accessors))
         (writers (set-difference (mapcar #'cadr (c2mop:slot-definition-writers this))
                                  accessors)))
    `(:class-slot ,(c2mop:slot-definition-name this)
      :accessors ,accessors
      :readers ,readers
      :writers ,writers
      :documentation ,(documentation this t))))

@export
(defmethod generate-documentation ((this <doc-variable>))
  `(,(doc-type this) ,(doc-name this)
    :documentation ,(documentation this 'variable)))
