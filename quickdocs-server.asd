(in-package :cl-user)
(defpackage quickdocs-server-asd
  (:use :cl :asdf))
(in-package :quickdocs-server-asd)

(defsystem quickdocs-server
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on (:clack
               :caveman2
               :envy
               :osicat
               :cl-ppcre
               :cl-annot
               :quickdocs-util

               ;; HTML Template
               :cl-emb

               ;; for rendering JSON
               :yason
               :trivial-types

               ;; for CL-DBI
               :caveman2-db

               :alexandria
               :flexi-streams)
  :components ((:module "server/src"
                :components
                ((:file "server" :depends-on ("config"))
                 (:file "web" :depends-on ("view" "model" "search"))
                 (:file "view" :depends-on ("config"))
                 (:file "search" :depends-on ("model"))
                 (:file "config")
                 (:module "model"
                  :components
                  ((:file "project" :depends-on ("system"))
                   (:file "system"))
                  :depends-on ("config")))))
  :description ""
  :in-order-to ((test-op (load-op quickdocs-server-test))))
