#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage quickdocs-asd
  (:use :cl :asdf))
(in-package :quickdocs-asd)

(defsystem quickdocs
  :version "0.1-SNAPSHOT"
  :author "Eitarow Fukamachi"
  :license "LLGPL"
  :depends-on (:cl-annot
               :flexi-streams
               :alexandria

               ;; for parser
               :closer-mop
               :cl-ppcre

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

               ;; for search
               :split-sequence
               )
  :components ((:module "parser"
                :pathname "src/parser"
                :components
                ((:file "class" :depends-on ("util"))
                 (:file "util")
                 (:file "asdf" :depends-on ("class"))
                 (:file "parser" :depends-on ("class" "asdf" "util"))))
               (:module "renderer"
                :pathname "src/renderer"
                :depends-on ("parser")
                :components
                ((:file "renderer" :depends-on ("readme" "repository"))
                 (:file "readme")
                 (:file "repository")))
               (:module "server"
                :pathname "src/server"
                :depends-on ("renderer" "search")
                :components
                ((:file "server")))
               (:module "search"
                :pathname "src/search"
                :components
                ((:file "search")))))
