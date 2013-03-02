(in-package :cl-user)
(defpackage quickdocs.server
  (:use :cl)
  (:import-from :ningle
                :route
                :next-route
                :*response*)
  (:import-from :alexandria
                :when-let)
  (:import-from :local-time
                :timestamp-to-unix
                :now)
  (:import-from :clack
                :clackup
                :stop)
  (:import-from :clack.builder
                :builder)
  (:import-from :clack.response
                :headers
                :redirect)
  (:import-from :clack.middleware.static
                :<clack-middleware-static>)
  (:import-from :quickdocs.quicklisp
                :ql-release-version)
  (:import-from :quickdocs.renderer
                :render-documentation
                :render-api-reference
                :static-path
                :template-path
                :render-with-layout)
  (:import-from :quickdocs.renderer.readme
                :project-readme)
  (:import-from :quickdocs.search
                :search-projects
                :*ql-download-stats-hash*)
  (:import-from :quickdocs.server.app
                :<app>
                :error-log))
(in-package :quickdocs.server)

(cl-annot:enable-annot-syntax)

(defun build (app)
  (builder
   (<clack-middleware-static>
    :path (lambda (path)
            (when (ppcre:scan "^(?:/images/|/css/|/js/|/html/|/robot\\.txt$|/favicon.ico$)" path)
              path))
    :root (asdf:system-relative-pathname
           :quickdocs #p"static/"))
   app))

(defvar *app-env* nil)
(defvar *app* (make-instance '<app>))
(defvar *deploy-time* nil)

(setf (route *app* "*")
      #'(lambda (params)
          (declare (ignore params))
          (setf (headers *response* :content-type) "text/html")
          (or (next-route)
              `(404
                (:content-type "text/html")
                ,(static-path "404.html")))))

(setf (route *app* "/")
      #'(lambda (params)
          (declare (ignore params))
          (emb:execute-emb
           (template-path "index.tmpl")
           :env `(:count
                  (:releases ,(length (ql-dist:provided-releases t))
                   :systems  ,(length (ql-dist:provided-systems t)))
                  :dist-version ,(slot-value (ql-dist:dist "quicklisp") 'ql-dist:version)))))

(setf (route *app* "/:project-name/")
      #'(lambda (params)
          (let ((release (ql-dist:find-release (getf params :project-name))))
            (if release
                (render-documentation release)
                (next-route)))))

(setf (route *app* "/:project-name/api")
      #'(lambda (params)
          (let ((release (ql-dist:find-release (getf params :project-name))))
            (if release
                (if (eq *app-env* :production)
                    (multiple-value-bind (stdout stderr)
                        (trivial-shell:shell-command
                         (format nil "LANG=en_US.UTF-8 ~A ~A"
                          (asdf:system-relative-pathname :quickdocs #P"bin/render")
                          (getf params :project-name))
                         :input "")
                      (princ stderr *error-output*)
                      stdout)
                    (render-api-reference release))
                (next-route)))))

(setf (route *app* "/search")
      #'(lambda (params)
          (let ((query (if (string= "" (getf params :|q|))
                           nil
                           (getf params :|q|))))
            (render-with-layout
             :title (if query
                        "Search Results | Quickdocs"
                        "All Projects | Quickdocs")
             :query query
             :content
             (emb:execute-emb
              (template-path "search.tmpl")
              :env `(:releases ,(loop for release in (search-projects query)
                                      for project-name = (slot-value release 'ql-dist:project-name)
                                      collect `(:name ,project-name
                                                :ql-version ,(ql-release-version release)
                                                :download-count
                                                ,(gethash project-name *ql-download-stats-hash*)
                                                :readme ,(project-readme (car (ql-dist:provided-systems release)))))
                     :query ,query))))))

;; Redirect to a project page.
(setf (route *app* "/:project-name")
      #'(lambda (params)
          (redirect *response*
                    (format nil "/~A/" (getf params :project-name))
                    301)
          ""))

(let (handler)
  @export
  (defun start-server (&rest args &key mode port server debug error-log &allow-other-keys)
    (declare (ignorable mode port server debug error-log))
    (let ((args (loop for (key val) on args by #'cddr
                      unless (find key '(:mode :error-log))
                        append (list key val))))
      (setf (error-log *app*) error-log)
      (setf *app-env* mode)
      (setf *deploy-time*
            (local-time:timestamp-to-unix (local-time:now)))
      (setf handler
            (apply #'clackup (build *app*) args))))

  @export
  (defun stop-server ()
    (stop handler)
    (setf handler nil)))
