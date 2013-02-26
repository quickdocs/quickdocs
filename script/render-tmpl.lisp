#!/usr/bin/env sbcl --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(let ((*standard-output* (make-broadcast-stream))
      (*trace-output* (make-broadcast-stream)))
  (ql:quickload :cl-emb)
  (ql:quickload :cl-fad))

(defparameter *template-path*
              (merge-pathnames
               "../templates/"
               (directory-namestring #.(or *load-pathname* *compile-file-pathname*))))

(defun template-path (file)
  (merge-pathnames file *template-path*))

(defun render (file)
  (emb:execute-emb (template-path #P"layout-for-error.tmpl")
   :env `(:content
          ,(emb:execute-emb (template-path file)))))

(defun main (file)
  (cond
    ((null file)
     (format *error-output* "~&Usage: $ render-tmpl.lisp <filename>~%"))
    ((null (fad:file-exists-p (template-path file)))
     (format *error-output* "~&~A: File doesn't exist.~%"
             file))
    (t (princ (render file)))))

(main (cadr sb-ext:*posix-argv*))