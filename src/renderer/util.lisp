(in-package :cl-user)
(defpackage quickdocs.renderer.util
  (:use :cl)
  (:import-from :flexi-streams
                :octets-to-string
                :with-output-to-sequence)
  (:import-from :alexandria
                :copy-stream))
(in-package :quickdocs.renderer.util)

(cl-annot:enable-annot-syntax)

@export
(defun slurp-file (file)
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (flex:octets-to-string
     (flex:with-output-to-sequence (out)
      (alexandria:copy-stream in out :finish-output t))
     :external-format :utf-8)))