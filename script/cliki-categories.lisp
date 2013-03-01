#!/usr/bin/env sbcl --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :drakma)
(ql:quickload :hunchentoot) ; for url-decode
(ql:quickload :cl-ppcre)

(defparameter *data-path*
              (asdf:system-relative-pathname :quickdocs #P"data/cliki/categories.txt"))

(defun retrieve-project-page (project-name)
  (loop with try = 1
        for (body status) = (multiple-value-list
                             (drakma:http-request (format nil "http://cliki.net/~A"
                                                   (drakma:url-encode project-name :utf-8))
                              :connection-timeout 60
                              :preserve-uri t))
        until (>= try 5)
        if (= status 200)
          do (return body)
        else if (= status 404)
          do (return nil)
        else
          do (incf try)
             (sleep (* 3 try))
        finally
        (format *error-output* "~&Failed to retrieve a CLiki page of ~A.~%"
                project-name)))

(defvar *category-anchor-scanner*
    (ppcre:create-scanner "<a\\s+href=\"[^\"]+\"\\s+class=\"category\""))
(defvar *href-scanner* (ppcre:create-scanner "href=\"/?([^\"]+)\""))

(defun parse-categories (html)
  (mapcar
   #'(lambda (m)
       (hunchentoot:url-decode
        (aref (nth-value 1 (ppcre:scan-to-strings *href-scanner* m)) 0)))
   (ppcre:all-matches-as-strings *category-anchor-scanner* html)))

(loop with i = 0
      with provided-releases = (ql-dist:provided-releases (ql-dist:dist "quicklisp"))
      with releases-count = (length provided-releases)
      for release in provided-releases
      for release-name = (slot-value release 'ql-dist:project-name)
      for body = (retrieve-project-page release-name)
      for categories = (and body (parse-categories body))
      collect (cons release-name categories) into data
      do (format t "~&[~4D/~4D] ~A: ~:[None~;~:*~{~A~^, ~}~]~%"
                 (incf i)
                 releases-count
                 release-name
                 categories)
         (sleep 3)
      finally
      (with-open-file (out *data-path*
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
        (pprint data out))
      (format t "~&Done.~%"))
