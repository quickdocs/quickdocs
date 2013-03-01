(in-package :cl-user)
(defpackage quickdocs.test.parse
  (:use :cl
        :cl-test-more
        :quickdocs.parser))
(in-package :quickdocs.test.parse)

(plan nil)

(loop for system-name in `(:clack
                           :alexandria
                           :lambda-reader ; utf-8 symbol
                           :xcvb          ; overriding another package
                           :elephant      ; c-file-source
                           #+sbcl
                           ,@(when (asdf:version-satisfies (lisp-implementation-version) "1.0.56")
                               '(:quid-pro-quo))
                           :s-xml         ; reader macro
                           )
      for system = (asdf:find-system system-name)
      for parsed = (progn
                     (diag (format nil "Parsing system: ~A" system-name))
                     (parse-documentation system))
      do (ok parsed "can parse")
         (ok (getf parsed :package-list) "has some packages"))

(finalize)
