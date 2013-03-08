#!/usr/bin/env sbcl --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :drakma)
(ql:quickload :hunchentoot) ; for url-decode
(ql:quickload :cl-ppcre)
(ql:quickload :html-entities)

(defparameter *data-path*
              (asdf:system-relative-pathname :quickdocs #P"data/cliki/"))

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

(defvar *body-scanner*
    (ppcre:create-scanner "<body>.+</body>" :single-line-mode t :case-insensitive-mode t))
(defun parse-body (html)
  (ppcre:scan-to-strings *body-scanner* html))

(defvar *category-anchor-scanner*
    (ppcre:create-scanner "<a\\s+href=\"[^\"]+\"\\s+class=\"category\""))
(defvar *href-scanner* (ppcre:create-scanner "href=\"/?([^\"]+)\""))

(defun parse-categories (html)
  (mapcar
   #'(lambda (m)
       (hunchentoot:url-decode
        (aref (nth-value 1 (ppcre:scan-to-strings *href-scanner* m)) 0)))
   (ppcre:all-matches-as-strings *category-anchor-scanner* html)))

(defvar *description-scanner*
    (ppcre:create-scanner "<div id=\"article\">(.+?)<(?:p|/?div)>" :single-line-mode t :case-insensitive-mode t))

(defun parse-description (html)
  (let ((description (aref (nth-value 1 (ppcre:scan-to-strings *description-scanner* html)) 0)))
    (html-entities:decode-entities
     (ppcre:regex-replace-all "\\s+"
      ;; XXX: dirty hack to emit HTML tags.
      (ppcre:regex-replace-all "<.+?>" description "")
      " "))))

(defvar *systems*
    (or (mapcar #'ql-dist:find-release (cdr sb-ext:*posix-argv*))
        (ql-dist:provided-releases (ql-dist:dist "quicklisp"))))

(loop with i = 0
      with provided-releases = *systems*
      with releases-count = (length provided-releases)
      for release in provided-releases
      for release-name = (slot-value release 'ql-dist:project-name)
      for body = (retrieve-project-page release-name)
      for categories = (and body (parse-categories body))
      for description = (and body (parse-description body))
      collect (cons release-name categories) into data-categories
      collect (cons release-name description) into data-description
      do (format t "~&[~4D/~4D] ~A: ~:[None~;~:*~A (~{~A~^, ~})~]~%"
                 (incf i)
                 releases-count
                 release-name
                 description
                 categories)
         (sleep 3)
      finally
   (unless (cadr sb-ext:*posix-argv*)
     (with-open-file (out (merge-pathnames #P"categories.txt" *data-path*)
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
       (pprint data-categories out))
     (with-open-file (out (merge-pathnames #P"description.txt" *data-path*)
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
       (pprint data-description out)))
   (format t "~&Done.~%"))
