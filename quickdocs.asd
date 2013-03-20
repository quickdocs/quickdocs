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

               ;; quickdocs
               :quickdocs-parser
               :quickdocs-renderer
               )
  :components ((:file "quicklisp"
                :pathname "src/quicklisp")
               (:module "server"
                :pathname "src/server"
                :depends-on ("search" "quicklisp")
                :components
                ((:file "server" :depends-on ("app"))
                 (:file "app")))
               (:module "search"
                :pathname "src/search"
                :components
                ((:file "search")))))
