#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.doc
  (:use :cl)
  (:import-from :cl-markdown
                :markdown)
  (:import-from :clack.doc.class
                :generate-documentation
                :find-system-packages
                :doc-name)
  (:import-from :clack.doc.asdf
                :ensure-system-loaded)
  (:import-from :cl-fad
                :copy-file))
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
               :long-description ,(ppcre:regex-replace-all "(<h\\d+>([^<]+))" long-description
                                   (lambda (match whole name)
                                     (format nil "<a name=\"~A\"></a>~A"
                                             (clack.util.hunchentoot:url-encode name)
                                             whole))
                                   :simple-calls t)
               :toc ,(parse-toc long-description)
               :package-list ,(mapcar #'doc-name (reverse packages))))
       stream)))
  (fad:copy-file (asdf:system-relative-pathname :clack-doc "view/main.css") "main.css" :overwrite t)
  t)

(defun parse-toc (xml)
  (loop for tag in (cddr (xmls:parse (format nil "<body>~A</body>" xml)))
        with str = (make-string-output-stream)
        if (and (not (string-equal "h1" (car tag)))
                (ppcre:scan "^h\\d+$" (car tag)))
          do
       (dotimes (i (* 4 (- (parse-integer (subseq (car tag) 1)) 2)))
         (princ " " str))
       (princ "- " str)
       (format str "[~A](#~A)" (third tag) (clack.util.hunchentoot:url-encode (third tag)))
       (fresh-line str)
        finally (return (nth-value 1 (markdown (get-output-stream-string str) :stream nil)))))
