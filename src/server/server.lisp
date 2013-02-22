#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.doc.server
  (:use :cl)
  (:import-from :ningle
                :<app>
                :route)
  (:import-from :clack
                :clackup
                :stop)
  (:import-from :clack.builder
                :builder)
  (:import-from :clack.middleware.static
                :<clack-middleware-static>)
  (:import-from :clack.doc.renderer
                :render-documentation
                :template-path
                :render-with-layout))
(in-package :clack.doc.server)

(cl-annot:enable-annot-syntax)

(defun build (app)
  (builder
   (<clack-middleware-static>
    :path (lambda (path)
            (when (ppcre:scan "^(?:/static/|/images/|/css/|/js/|/robot\\.txt$|/favicon.ico$)" path)
              (ppcre:regex-replace "^/static" path "")))
    :root (asdf:system-relative-pathname
           :clack-doc #p"static/"))
   app))

(defvar *app* (make-instance '<app>))

(setf (route *app* "/project/:project-name")
      #'(lambda (params)
          (let ((release (ql-dist:find-release (getf params :project-name))))
            (if release
                (render-documentation release)
                '(404 () ("not found"))))))

(setf (route *app* "/search")
      #'(lambda (params)
          (let* ((query (or (getf params :|q|) ""))
                 (re (ppcre:create-scanner (ppcre:quote-meta-chars query)))
                 (releases
                  (remove-if-not
                   #'(lambda (release)
                       (ppcre:scan re (slot-value release 'ql-dist:project-name)))
                   (ql-dist:provided-releases t))))
            (render-with-layout
             :title "Search Results | ClackDoc"
             :query query
             :content
             (emb:execute-emb
              (template-path "search.tmpl")
              :env `(:releases ,(mapcar #'(lambda (release)
                                            (slot-value release 'ql-dist:project-name))
                                 releases)
                     :query ,(getf params :|q|)))))))

(let (handler)
  @export
  (defun start-server ()
    (setf handler (clackup (build *app*))))

  @export
  (defun stop-server ()
    (stop handler)
    (setf handler nil)))
