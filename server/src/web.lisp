(in-package :cl-user)
(defpackage quickdocs.server.web
  (:use :cl
        :caveman2
        :caveman2.db
        :quickdocs.server.config
        :quickdocs.server.view
        :quickdocs.server.search
        :quickdocs.model.project
        :quickdocs.model.system)
  (:export :*web*))
(in-package :quickdocs.server.web)

(cl-annot:enable-annot-syntax)

;;
;; Application

(defclass <web> (<app>) ())
(defparameter *web* (make-instance '<web>))

;;
;; Routing rules

(defun ql-dist-version ()
  (or (getf (select-one (connect-db) :value
              (from :preference)
              (where (:= :name "ql-dist-version"))
              (limit 1))
            :|value|)
      (ql-dist:version (ql-dist:dist "quicklisp"))))

@route GET "/"
(defun index ()
  (with-layout (:title "Quickdocs"
                :css "/css/index.css")
    (render #P"index.tmpl"
            `(:count
              (:releases ,(length (ql-dist:provided-releases t))
               :systems  ,(length (ql-dist:provided-systems t)))
              :dist-version ,(ql-dist-version)))))

@route GET "/:project-name/"
(defun project-page (&key project-name)
  (let* ((dist-version (ql-dist-version))
         (project (find-project project-name dist-version)))
    (unless project
      (throw-code 404))

    (with-layout (:title (format nil "~A | Quickdocs" project-name)
                  :css "/css/main.css")
      (render #P"project.tmpl"
              (list
               :name project-name
               :ql-version (project-release-version project)
               :archive-url (project-archive-url project)
               :project-url (project-repos-url project)
               :homepage (project-homepage-url project)
               :readme (getf (project-readme project) :|converted|)
               :authors (mapcar
                         (lambda (author) (getf author :|author_name|))
                         (project-authors project))
               :maintainers (mapcar
                             (lambda (author) (getf author :|author_name|))
                             (project-maintainers project))
               :licenses (project-licenses project)
               :dependencies (project-dependencies project)
               :depending (depending-projects project)
               :categories (project-categories project))))))

@route GET "/:project-name/api"
(defun project-api-reference (&key project-name)
  (let* ((dist-version (ql-dist-version))
         (project (find-project project-name dist-version)))
    (unless project
      (throw-code 404))

    (with-layout (:title (format nil "~A | API Reference | Quickdocs" project-name)
                  :css "/css/main.css")
      (render #P"api.tmpl"
              (list
               :name project-name
               :archive-url (project-archive-url project)
               :project-url (project-repos-url project)
               :homepage (project-homepage-url project)
               :system-list
               (remove nil
                       (mapcar
                        #'(lambda (system) (system-parsed-data system dist-version))
                        (project-systems project)))
               :errors
               (remove nil
                       (mapcar
                        #'(lambda (system) (system-parse-error system dist-version))
                        (project-systems project))))))))

@route GET "/search"
(defun search-page (&key |q|)
  (let ((projects (search-projects |q| (ql-dist-version))))

    ;; Ensure descriptions are loaded.
    (map nil #'project-description projects)

    (with-layout (:title (if |q|
                             "Search Results | Quickdocs"
                             "All Projects | Quickdocs")
                  :css "/css/main.css")
      (render #P"search.tmpl"
              (list
               :releases projects
               :query |q|)))))

@route GET "/:project-name"
(defun redirect-to-project (&key project-name)
  (redirect (format nil "/~A/" project-name) 301)
  "")

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
