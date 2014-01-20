(in-package :cl-user)
(defpackage quickdocs-updater-asd
  (:use :cl :asdf))
(in-package :quickdocs-updater-asd)

(defsystem quickdocs-updater
  :version "0.1"
  :author "Eitarow Fukamachi"
  :depends-on (:quickdocs-parser
               :quickdocs-util
               :lparallel
               :flexi-streams
               :alexandria
               :drakma
               :memoize
               :caveman2-db

               ;; for url-encode, url-decode
               :hunchentoot
               :html-entities)
  :components ((:module "updater"
                :components
                ((:file "project" :depends-on ("model"))
                 (:file "cliki")
                 (:module "model"
                  :components
                  ((:file "project" :depends-on ("system"))
                   (:file "system"))
                  :depends-on ("util"))
                 (:file "util")))))
