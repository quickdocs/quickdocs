#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.doc
  (:use :cl
        :anaphora)
  (:import-from :cl-markdown
                :markdown)
  (:import-from :clack.doc.class
                :generate-documentation
                :find-system-packages
                :doc-name)
  (:import-from :clack.doc.asdf
                :ensure-system-loaded)
  (:import-from :cl-fad
                :copy-file)
  (:import-from :xmls
                :parse)
  (:import-from :cl-ppcre
                :scan-to-strings)
  (:import-from :cl-emb
                :url-encode))
(in-package :clack.doc)

(cl-annot:enable-annot-syntax)

@export
(defmethod generate-documentation ((system asdf:system))
  (ensure-system-loaded system)
  (let ((packages (find-system-packages system))
        (*print-case* :downcase)
        (long-description (nth-value 1
                                     (markdown (ignore-errors (slot-value system 'asdf::long-description))
                                               :stream nil))))
    (loop for pkg in (reverse packages)
          do (with-open-file (stream (format nil "~(~A~).html" (doc-name pkg))
                                     :direction :output
                                     :if-exists :supersede)
               (write-string (generate-documentation pkg) stream)))
    (with-open-file (stream "index.html"
                            :direction :output
                            :if-exists :supersede)
      (write-string
       (emb:execute-emb
        (asdf:system-relative-pathname :clack-doc "view/system.tmpl")
        :env `(:name ,(string-capitalize (slot-value system 'asdf::name))
               :description ,(ignore-errors (slot-value system 'asdf::description))
               :sections ,(xml->sections long-description)
               :package-list ,(mapcar #'doc-name (reverse packages))))
       stream)))
  (fad:copy-file (asdf:system-relative-pathname :clack-doc "view/main.css") "main.css" :overwrite t)
  t)

(defun header-tag-p (tag)
  (awhen (nth-value 1 (ppcre:scan-to-strings "^h(\\d+)$" (car tag)))
    (parse-integer (aref it 0))))

(defun xml->sections (xml)
  (loop with current-section = nil
        with current = nil
        for tag in (cddr (xmls:parse (format nil "<dummy>~A</dummy>" xml) :compress-whitespace nil))
        for level = (header-tag-p tag)
        if level
          ;; new section
          collect `(:level ,(getf current :level) :title ,(getf current :title) :body ,(apply #'concatenate 'string (mapcar (lambda (tag) (xmls:write-xml tag nil)) (reverse current-section)))) into sections
          and do (setf current-section nil)
          and do (setf current `(:level ,level :title ,(third tag)))
        else
          do (push tag current-section)
        finally
     (return (cdr `(,@sections (:level ,(getf current :level) :title ,(getf current :title) :body ,(apply #'concatenate 'string (mapcar (lambda (tag) (xmls:write-xml tag nil)) (reverse current-section)))))))))
