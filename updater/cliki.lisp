(in-package :cl-user)
(defpackage quickdocs.updater.cliki
  (:use :cl
        :caveman2.db)
  (:import-from :drakma
                :http-request)
  (:import-from :hunchentoot
                :url-encode
                :url-decode)
  (:import-from :ppcre
                :create-scanner
                :scan-to-strings
                :all-matches-as-strings)
  (:import-from :html-entities
                :decode-entities))
(in-package :quickdocs.updater.cliki)

(cl-annot:enable-annot-syntax)

@export
(defun retrieve-project-page (project-name)
  (loop with try = 1
        for (body status) = (multiple-value-list
                             (drakma:http-request (format nil "http://cliki.net/~A"
                                                   (hunchentoot:url-encode project-name :utf-8))
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

@export
(defun map-cliki-project-pages (db dist-version fn)
  (loop with i = 0
        with projects = (select-all db (:id :name)
                          (from :project)
                          (where (:= :ql_dist_version dist-version)))
        with count = (length projects)
        for project in projects
        for body = (retrieve-project-page (getf project :|name|))
        do (format t "~&[~4D/~4D] ~A~:[: None~;~]~%"
                   (incf i)
                   count
                   (getf project :|name|)
                   body)
           (when body
             (funcall fn project body))
           (sleep 3)))

@export
(defun update-cliki-informations (db &optional (dist-version
                                                (ql-dist:version (ql-dist:dist "quicklisp"))))
  (map-cliki-project-pages
   db dist-version
   #'(lambda (project body)
       (let ((description (parse-description body)))
         (delete-from db :project_cliki_description
           (where (:= :project_id (getf project :|id|))))
         (insert-into db :project_cliki_description
           (set= :project_id (getf project :|id|)
                 :description description)))

       (let ((categories (parse-categories body)))
         (delete-from db :project_category
           (where (:= :project_name (getf project :|name|))))
         (loop for category in categories
               do (insert-into db :project_category
                    (set= :project_name (getf project :|name|)
                          :category category)))))))

;;
;; Description

(defvar *description-scanner*
    (ppcre:create-scanner "<div id=\"article\">(.+?)<(?:p|/?div)>" :single-line-mode t :case-insensitive-mode t))

(defun parse-description (html)
  (let ((description (aref (nth-value 1 (ppcre:scan-to-strings *description-scanner* html)) 0)))
    (html-entities:decode-entities
     (ppcre:regex-replace-all "\\s+"
      ;; XXX: dirty hack to emit HTML tags.
      (ppcre:regex-replace-all "<.+?>" description "")
      " "))))

;;
;; Category

(defvar *category-anchor-scanner*
    (ppcre:create-scanner "<a\\s+href=\"[^\"]+\"\\s+class=\"category\""))
(defvar *href-scanner* (ppcre:create-scanner "href=\"/?([^\"]+)\""))

(defun parse-categories (html)
  (remove-duplicates
   (mapcar
    #'(lambda (m)
        (hunchentoot:url-decode
         (aref (nth-value 1 (ppcre:scan-to-strings *href-scanner* m)) 0)))
    (ppcre:all-matches-as-strings *category-anchor-scanner* html))
   :test #'string-equal))
