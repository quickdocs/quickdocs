#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.doc.generator
  (:use :cl
        :clack.doc.class))
(in-package :clack.doc.generator)

(defun gendoc (type summary &optional description)
  (when description
    (setf description (string-trim #(#\Newline) description)))
  (format nil "- ~:(~A~): ~A~:[~;~:*<pre>~{~/clack.doc.markdown:markdown-escape/~^<br />~}</pre>~]
"
          type summary (and description
                            (split-sequence #\Newline description))))

@export
(defmethod generate-documentation ((this <doc-package>))
  (format nil
          "~2&~A~2&## EXTERNAL SYMBOLS~2%<div class=\"symbol\">~2%~{~A~}</div>"
         (or (documentation (find-entity this) t)
             (format nil "# ~A~%" (string-capitalize (doc-name this))))
         (mapcar #'generate-documentation
                 (reverse
                  (remove-if-not
                   #'externalp
                   (package-symbols this))))))

@export
(defmethod generate-documentation ((this <doc-function>))
  (gendoc (doc-type this)
          (format nil "<strong>~(~/clack.doc.markdown:markdown-escape/~)</strong>~:[~;~:* [~{~(~/clack.doc.markdown:markdown-escape/~)~^ ~}]~]"
                  (doc-name this)
                  (normalized-lambda-list this))
          (documentation (doc-name this) 'function)))

@export
(defmethod generate-documentation ((this <doc-method>))
  (gendoc (doc-type this)
          (format nil "<strong>~(~/clack.doc.markdown:markdown-escape/~)</strong>~:[~;~:* [~{~(~/clack.doc.markdown:markdown-escape/~)~^ ~}]~]"
                  (doc-name this)
                  (normalized-lambda-list this))
          (documentation (find-entity this) t)))

@export
(defmethod generate-documentation ((this <doc-class>))
  (prepare this)
  (concatenate
   'string
   (gendoc (doc-type this)
           (format nil "<strong>~(~/clack.doc.markdown:markdown-escape/~)</strong>~:[~;~:* inherits ~(~/clack.doc.markdown:markdown-escape/~)~]"
                   (doc-name this)
                   (class-super-classes this)
                   )
           (documentation (doc-name this) 'type))
   (format nil "~:[~;~:*~2&<dl>~{~A~}</dl>~]~%"
           (mapcar #'generate-documentation
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
  (gendoc (doc-type this)
          (format nil "<strong>~/clack.doc.markdown:markdown-escape/</strong>"
                  (string-downcase (doc-name this)))
          (documentation this 'variable)))
