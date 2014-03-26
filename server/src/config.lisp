(in-package :cl-user)
(defpackage quickdocs.server.config
  (:use :cl)
  (:import-from :envy
                :config-env-var
                :defconfig)
  (:export :config
           :*project-root*
           :*application-root*
           :*static-directory*
           :*template-directory*
           :*data-directory*))
(in-package :quickdocs.server.config)

(setf (config-env-var) "APP_ENV")

(defparameter *project-root*       (asdf:system-source-directory :quickdocs-server))
(defparameter *application-root*   (merge-pathnames #P"server/" *project-root*))
(defparameter *static-directory*   (merge-pathnames #P"static/" *application-root*))
(defparameter *template-directory* (merge-pathnames #P"templates/" *application-root*))
(defparameter *data-directory*     (merge-pathnames #P"data/" *project-root*))

(defconfig |default|
  `(:databases ((:maindb :mysql :database-name "quickdocs"
                                :username "nobody"
                                :password "nobody"))))

(defconfig |staging|
  `(:error-log #P"/var/log/apps/quickdocs_stage_error.log"
    ,@|default|))

(defconfig |production|
  `(:error-log #P"/var/log/apps/quickdocs_error.log"
    ,@|default|))

(defun config (&optional key)
  (envy:config #.(package-name *package*) key))
