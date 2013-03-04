(in-package :cl-user)
(defpackage quickdocs-asd
  (:use :cl :asdf))
(in-package :quickdocs-asd)

(defsystem quickdocs
  :version "0.1-SNAPSHOT"
  :author "Eitarow Fukamachi"
  :depends-on (:cl-annot
               :flexi-streams
               :alexandria

               :quickdocs-parser

               ;; for renderer
               :cl-emb
               :cl-markdown
               :trivial-shell
               :cl-fad
               :yason
               :drakma
               :memoize
               :trivial-backtrace

               ;; for server
               :clack
               :ningle
               :local-time

               ;; for search
               :split-sequence
               )
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
                ((:file "renderer" :depends-on ("readme" "repository" "category"))
                 (:file "readme" :depends-on ("util"))
                 (:file "repository" :depends-on ("util"))
                 (:file "category" :depends-on ("util"))
                 (:file "util")))
               (:module "server"
                :pathname "src/server"
                :depends-on ("renderer" "search" "quicklisp")
                :components
                ((:file "server" :depends-on ("app"))
                 (:file "app")))
               (:module "search"
                :pathname "src/search"
                :depends-on ("renderer") ; quickdocs.renderer.category
                :components
                ((:file "search")))))
