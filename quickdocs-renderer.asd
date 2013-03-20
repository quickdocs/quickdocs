(in-package :cl-user)
(defpackage quickdocs-renderer-asd
  (:use :cl :asdf))
(in-package :quickdocs-renderer-asd)

(defsystem quickdocs-renderer
  :version "0.1-SNAPSHOT"
  :author "Eitarow Fukamachi"
  :depends-on (:cl-annot
               :alexandria

               :cl-emb
               :cl-markdown
               :trivial-shell
               :cl-fad
               :yason
               :drakma
               :memoize
               :trivial-backtrace

               :quickdocs-parser)
  :components ((:file "quicklisp"
                :pathname "src/quicklisp")
               (:module "model"
                :pathname "src/model"
                :components
                ((:file "project")))
               (:module "renderer"
                :pathname "src/renderer"
                :depends-on ("quicklisp" "model")
                :components
                ((:file "renderer" :depends-on ("readme" "repository" "category" "description"))
                 (:file "readme" :depends-on ("util"))
                 (:file "repository" :depends-on ("util"))
                 (:file "category" :depends-on ("util"))
                 (:file "description" :depends-on ("util"))
                 (:file "util")))))
