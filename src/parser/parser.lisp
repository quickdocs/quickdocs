(in-package :cl-user)
(defpackage quickdocs.parser
  (:use :cl
        :quickdocs.parser.class)
  (:import-from :alexandria
                :when-let)
  (:import-from :cl-ppcre
                :regex-replace
                :quote-meta-chars)
  (:import-from :quickdocs.parser.asdf
                :ensure-system-loaded)
  (:import-from :quickdocs.parser.util
                :slot-value*
                :with-ignoring-streams
                :map-tree)
  (:import-from :quickdocs.parser.serialize
                :serialize-symbol))
(in-package :quickdocs.parser)

(cl-annot:enable-annot-syntax)

@export
(defmethod parse-documentation ((system asdf:system))
  (with-ignoring-streams (*standard-output*
                          *debug-io*
                          *trace-output*)
    (ensure-system-loaded system))
  (let* ((name (slot-value* system 'asdf::name))
         (system-re (format nil "(?i)^~A" (ppcre:quote-meta-chars name))))
    `(:type :system
      :name ,(slot-value* system 'asdf::name)
      :author ,(slot-value* system 'asdf::author)
      :maintainer ,(slot-value* system 'asdf::maintainer)
      :version ,(slot-value* system 'asdf::version)
      :licence ,(slot-value* system 'asdf::licence)
      :description ,(slot-value* system 'asdf::description)
      :long-description ,(slot-value* system 'asdf::long-description)
      :depends-on ,(mapcar #'princ-to-string (asdf::component-load-dependencies system))
      :package-list
      ,(mapcar
        #'(lambda (plist)
            (unless (string-equal (getf plist :name) name)
              (setf (getf plist :name)
                    (ppcre:regex-replace system-re (getf plist :full-name) ""
                     :preserve-case nil)))
            plist)
        (mapcar #'parse-documentation
         (reverse (find-system-packages system)))))))

@export
(defmethod parse-documentation ((system ql-dist:system))
  (unless (ql-dist:installedp system)
    (with-ignoring-streams (*standard-output*
                            *debug-io*
                            *trace-output*)
      (ql-dist:install system)))
  (when-let (asdf-system (with-ignoring-streams (*standard-output*
                                                 *debug-io*
                                                 *trace-output*)
                           (handler-case
                               (asdf:find-system (slot-value system 'ql-dist:name))
                             (asdf:load-system-definition-error (e)
                               (princ e *error-output*)
                               nil))))
    (parse-documentation asdf-system)))

@export
(defmethod parse-documentation ((this <doc-package>))
  `(:type :package
    :name ,(doc-name this)
    :full-name ,(doc-name this)
    :documentation ,(documentation (find-entity this) t)
    :symbol-list
    ,(mapcar #'parse-documentation
      (reverse
       (package-symbols this)))
    :external-symbols
    ,(when-let (package (find-entity this))
       (let (exported)
         (do-external-symbols (s package exported)
           (push (serialize-symbol s) exported))))))

@export
(defmethod parse-documentation ((this <doc-function>))
  `(:type ,(doc-type this)
    :symbol ,(etypecase (doc-name this)
               (symbol (serialize-symbol (doc-name this)))
               (list (mapcar #'serialize-symbol
                             (doc-name this))))
    :lambda-list ,(map-tree #'serialize-symbol (function-lambda-list this))
    :documentation ,(documentation (doc-name this) 'function)))

@export
(defmethod parse-documentation ((this <doc-method>))
  `(:type ,(doc-type this)
    :symbol ,(etypecase (doc-name this)
               (symbol (serialize-symbol (doc-name this)))
               (list (mapcar #'serialize-symbol
                             (doc-name this))))
    :lambda-list ,(map-tree #'serialize-symbol (function-lambda-list this))
    :documentation ,(documentation (find-entity this) t)))

@export
(defmethod parse-documentation ((this <doc-class>))
  (prepare this)
  `(:type ,(doc-type this)
    :symbol ,(serialize-symbol (doc-name this))
    :super-class-list ,(mapcar #'serialize-symbol (class-super-classes this))
    :documentation ,(documentation (doc-name this) 'type)
    :slot-list ,(mapcar #'parse-documentation
                 (class-slots this))))

#+(or sbcl ccl)
(defmethod parse-documentation ((this #+sbcl sb-mop:slot-definition
                                      #+ccl  ccl:slot-definition))
  (let* ((accessors (intersection (c2mop:slot-definition-readers this)
                                  (mapcar #'cadr (c2mop:slot-definition-writers this))))
         (readers (set-difference (c2mop:slot-definition-readers this)
                                  accessors))
         (writers (set-difference (mapcar #'cadr (c2mop:slot-definition-writers this))
                                  accessors)))
    `(:type :class-slot
      :symbol ,(serialize-symbol (c2mop:slot-definition-name this))
      :accessors ,(mapcar #'serialize-symbol accessors)
      :readers ,(mapcar #'serialize-symbol readers)
      :writers ,(mapcar #'serialize-symbol writers)
      :documentation ,(documentation this t))))

@export
(defmethod parse-documentation ((this <doc-variable>))
  `(:type ,(doc-type this)
    :symbol ,(serialize-symbol (doc-name this))
    :documentation ,(documentation (doc-name this) 'variable)
    ,@(if (initial-value-boundp this)
          `(:initial-value
            ,(prin1-to-string (doc-variable-initial-value this)))
          nil)))
