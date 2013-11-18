(in-package :cl-user)
(defpackage quickdocs.server.view
  (:use :cl)
  (:import-from :quickdocs.server.config
                :*template-directory*)
  (:import-from :caveman2
                :*response*)
  (:import-from :clack.response
                :headers)
  (:import-from :cl-emb
                :*escape-type*
                :*case-sensitivity*
                :*function-package*
                :execute-emb)
  (:import-from :yason
                :encode
                :encode-plist
                :encode-alist)
  (:import-from :trivial-types
                :property-list-p
                :association-list-p)
  (:export :*default-layout-path*
           :render
           :render-json
           :with-layout))
(in-package :quickdocs.server.view)

(defvar *default-layout-directory* #P"layouts/")
(defvar *default-layout-path* #P"default.tmpl")

(defun render (template-path &optional env)
  (let ((emb:*escape-type* :html)
        (emb:*case-sensitivity* nil))
    (emb:execute-emb
     (merge-pathnames template-path
                      *template-directory*)
     :env env)))

(defun render-json (object)
  (setf (headers *response* :content-type) "application/json")
  (with-output-to-string (s)
    (cond
      ((property-list-p object) (encode-plist object s))
      ((association-list-p object) (encode-alist object s))
      (T (encode object s)))))

(defmacro with-layout ((&rest env-for-layout) &body body)
  (let ((layout-path (merge-pathnames *default-layout-path*
                                      *default-layout-directory*)))
    (when (pathnamep (car env-for-layout))
      (setf layout-path (pop env-for-layout)))

    `(let ((emb:*escape-type* :html)
           (emb:*case-sensitivity* nil))
       (emb:execute-emb
        (merge-pathnames ,layout-path
                         *template-directory*)
        :env (list :content (progn ,@body)
                   ,@env-for-layout)))))

(defmacro comma (var)
  `(cl-emb::echo (format nil "~{~A~^, ~}"
                         (cl-emb::getf-emb ,(string var)))))

(defmacro downcase (var)
  `(cl-emb::echo (string-downcase (cl-emb::getf-emb ,(string var)))))

(defmacro capitalize (var)
  `(cl-emb::echo (string-capitalize (cl-emb::getf-emb ,(string var)))))

(defun slot (object slot-name)
  #+(or ccl cmu sbcl lispworks allegro clisp)
  (flet ((slot-definition-name (slot)
           (#+ccl ccl:slot-definition-name
            #+cmu pcl:slot-definition-name
            #+sbcl sb-mop:slot-definition-name
            #+lispworks hcl:slot-definition-name
            #+allegro mop:slot-definition-name
            #+clisp clos:slot-definition-name
            slot)))
    (let* ((slot (find
                  slot-name
                  (#+(and ccl (not openmcl-native-threads)) ccl:class-instance-slots
                     #+(and ccl openmcl-native-threads) ccl:class-slots
                     #+cmu pcl:class-slots
                     #+sbcl sb-mop:class-slots
                     #+lispworks hcl:class-slots
                     #+allegro mop:class-slots
                     #+clisp clos:class-slots
                     (class-of object))
                  :key #'slot-definition-name
                  :test #'string=))
           (slot-name (and slot (slot-definition-name slot))))
      (when (and slot-name
                 (slot-boundp object slot-name))
        (cl-emb::echo (slot-value object slot-name)))))
  #-(or ccl cmu sbcl lispworks allegro clisp)
  (error "`slot' in implementation ~S is not supported yet."
         (lisp-implementation-type)))

;; Define functions that are available in templates.
(import '(quickdocs.server.config:config
          caveman2:url-for
          comma
          downcase
          capitalize
          slot)
        emb:*function-package*)
