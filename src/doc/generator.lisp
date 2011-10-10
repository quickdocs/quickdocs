#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.doc.generator
  (:use :cl
        :clack.doc.class
        :split-sequence)
  (:import-from :cl-markdown
                :markdown))
(in-package :clack.doc.generator)

(cl-annot:enable-annot-syntax)

(defvar template-path (asdf:system-relative-pathname :clack-doc "view/"))

@export
(defmethod generate-documentation ((this <doc-package>))
  (emb:execute-emb
   (merge-pathnames "package.tmpl" template-path)
   :env `(:name ,(string-capitalize (doc-name this))
          :doc ,(nth-value 1 (markdown (documentation (find-entity this) t) :stream nil))
          :symbol-list
          ,(mapcar #'generate-documentation
            (reverse
             (remove-if-not
              #'externalp
              (package-symbols this)))))))

@export
(defmethod generate-documentation ((this <doc-function>))
  (emb:execute-emb
   (merge-pathnames "function.tmpl" template-path)
   :env `(:type ,(doc-type this)
          :name ,(doc-name this)
          :lambda-list ,(normalized-lambda-list this)
          :doc ,(documentation (doc-name this) 'function))))

@export
(defmethod generate-documentation ((this <doc-method>))
  (emb:execute-emb
   (merge-pathnames "function.tmpl" template-path)
   :env `(:type ,(doc-type this)
          :name ,(doc-name this)
          :lambda-list ,(normalized-lambda-list this)
          :doc ,(documentation (find-entity this) t))))

@export
(defmethod generate-documentation ((this <doc-class>))
  (prepare this)
  (emb:execute-emb
   (merge-pathnames "class.tmpl" template-path)
   :env `(:type ,(doc-type this)
          :name ,(string-downcase (doc-name this))
          :super-class-list ,(class-super-classes this)
          :doc ,(documentation (doc-name this) 'type)
          :slot-list ,(mapcar #'generate-documentation
                              (class-slots this)))))

(defmethod generate-documentation ((this c2mop:standard-direct-slot-definition))
  (let* ((accessors (intersection (c2mop:slot-definition-readers this)
                                  (mapcar #'cadr (c2mop:slot-definition-writers this))))
         (readers (set-difference (c2mop:slot-definition-readers this)
                                  accessors))
         (writers (set-difference (mapcar #'cadr (c2mop:slot-definition-writers this))
                                  accessors)))
    (format nil
            "<dt><strong>~(~A~)</strong>~:[~;~:* Accessor:~{ ~(~A~)~}~]~:[~;~:* Reader:~{ ~(~A~)~}~]~:[~;~:* Writer:~{ ~(~A~)~}~]</small></dt>~:[~;~:*<dd>~A</dd>~]"
            (c2mop:slot-definition-name this)
            accessors
            readers
            writers
            (documentation this t))))

@export
(defmethod generate-documentation ((this <doc-variable>))
  (emb:execute-emb
   (merge-pathnames "variable.tmpl" template-path)
   :env `(:type ,(doc-type this)
          :name ,(doc-name this)
          :doc ,(documentation this 'variable))))
