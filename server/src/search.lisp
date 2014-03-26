(in-package :cl-user)
(defpackage quickdocs.server.search
  (:use :cl
        :caveman2.db
        :quickdocs.model.project
        :split-sequence)
  (:import-from :ppcre
                :split)
  (:import-from :alexandria
                :ensure-list))
(in-package :quickdocs.server.search)

(cl-annot:enable-annot-syntax)

(defun sort-by-download-count (a b)
  (> (gethash (project-name a) *ql-download-stats-hash* 0)
     (gethash (project-name b) *ql-download-stats-hash* 0)))

@export
(defun search-projects (query &optional (dist-version
                                         (ql-dist:version (ql-dist:dist "quicklisp"))))
  (remove-duplicates
   (append
    (ensure-list (search-exact-project query dist-version))
    (sort (search-by-categories query dist-version) #'sort-by-download-count)
    (sort (search-by-name query dist-version) #'sort-by-download-count)
    (sort (search-by-description query dist-version) #'sort-by-download-count))
   :test #'eql
   :key #'project-id
   :from-end t))

(defun search-exact-project (query dist-version)
  (find-project query dist-version))

(defun search-by-name (query dist-version)
  (let ((queries (ppcre:split "\\s+" (string-downcase query))))
    (mapcar
     #'(lambda (row) (apply #'make-instance '<project> row))
     (select-all (connect-db) :*
       (from :project)
       (where `(:and (:= :ql_dist_version ,dist-version)
                     ,@(mapcar
                        (lambda (query) `(:like :name ,(format nil "%~A%" query)))
                        queries)))))))

(defun search-by-categories (query dist-version)
  (let* ((scanner (ppcre:create-scanner
                   (format nil "\\b~A\\b" (ppcre:quote-meta-chars query)) :case-insensitive-mode t))
         (project-categories (select-all (connect-db) :*
                               (from :project_category)
                               (where (:like (:lower :category) (format nil "%~(~A~)%" query)))))
         (categories (remove-if-not
                      (lambda (row)
                        (ppcre:scan scanner (getf row :|category|)))
                      project-categories)))
    (if categories
        (mapcar
         (lambda (row)
           (apply #'make-instance '<project> row))
         (select-all (connect-db) :*
           (from :project)
           (where (:and (:= :ql_dist_version dist-version)
                        (:in :name (mapcar (lambda (row) (getf row :|project_name|)) categories))))))
        nil)))

(defun search-by-description (query dist-version)
  (let ((queries (ppcre:split "\\s+" (string-downcase query))))
    (mapcar
     (lambda (row)
       (apply #'make-instance '<project> row))
     (select-all (connect-db) :project.*
       (from :project_cliki_description)
       (left-join :project :on (:= :project_cliki_description.project_id :project.id))
       (where `(:and (:= :ql_dist_version ,dist-version)
                     ,@(mapcar
                        (lambda (query) `(:like (:lower :description) ,(format nil "%~A%" query)))
                        queries)))))))
