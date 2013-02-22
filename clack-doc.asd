#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack-doc-asd
  (:use :cl :asdf))
(in-package :clack-doc-asd)

(defsystem clack-doc
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
               :cl-fad
               :yason
               :drakma

               ;; for server
               :clack
               :ningle
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
                :components
                ((:file "renderer" :depends-on ("readme" "repository"))
                 (:file "readme")
                 (:file "repository")))
               (:module "server"
                :pathname "src/server"
                :depends-on ("renderer")
                :components
                ((:file "server")))))
