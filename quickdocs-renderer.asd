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
  :components ((:module "renderer"
                :pathname "src/renderer"
                :components
                ((:file "renderer")))))
