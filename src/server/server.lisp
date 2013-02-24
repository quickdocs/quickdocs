(in-package :cl-user)
(defpackage quickdocs.server
  (:use :cl)
  (:import-from :ningle
                :<app>
                :route
                :next-route)
  (:import-from :clack
                :clackup
                :stop)
  (:import-from :clack.builder
                :builder)
  (:import-from :clack.middleware.static
                :<clack-middleware-static>)
  (:import-from :quickdocs.renderer
                :render-documentation
                :render-api-reference
                :template-path
                :render-with-layout)
  (:import-from :quickdocs.search
                :search-projects
                :sort-by-download-count))
(in-package :quickdocs.server)

(cl-annot:enable-annot-syntax)

(defun build (app)
  (builder
   (<clack-middleware-static>
    :path (lambda (path)
            (when (ppcre:scan "^(?:/static/|/images/|/css/|/js/|/robot\\.txt$|/favicon.ico$)" path)
              (ppcre:regex-replace "^/static" path "")))
    :root (asdf:system-relative-pathname
           :quickdocs #p"static/"))
   app))

(defvar *app* (make-instance '<app>))

(setf (route *app* "*")
      #'(lambda (params)
          (declare (ignore params))
          (or (next-route)
              `(404
                (:content-type "text/html")
                (,(render-with-layout :title "Quickdocs"
                                      :content (emb:execute-emb (template-path "404.tmpl"))))))))

(setf (route *app* "/")
      #'(lambda (params)
          (declare (ignore params))
          (emb:execute-emb
           (template-path "index.tmpl")
           :env `(:count
                  (:releases ,(length (ql-dist:provided-releases t))
                   :systems  ,(length (ql-dist:provided-systems t)))
                  :dist-version ,(slot-value (ql-dist:dist "quicklisp") 'ql-dist:version)))))

(setf (route *app* "/:project-name")
      #'(lambda (params)
          (let ((release (ql-dist:find-release (getf params :project-name))))
            (if release
                (render-documentation release)
                (next-route)))))

(setf (route *app* "/:project-name/api")
      #'(lambda (params)
          (let ((release (ql-dist:find-release (getf params :project-name))))
            (if release
                (render-api-reference release)
                (next-route)))))

(setf (route *app* "/-/search")
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
              :env `(:releases ,(mapcar #'(lambda (release)
                                            (slot-value release 'ql-dist:project-name))
                                 (sort (search-projects query) #'sort-by-download-count))
                     :query ,query))))))

(let (handler)
  @export
  (defun start-server (&rest args &key port server debug &allow-other-keys)
    (setf handler
          (apply #'clackup (build *app*) args)))

  @export
  (defun stop-server ()
    (stop handler)
    (setf handler nil)))
