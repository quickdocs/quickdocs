(in-package :cl-user)
(defpackage quickdocs-parser-asd
  (:use :cl :asdf))
(in-package :quickdocs-parser-asd)

(defsystem quickdocs-parser
  :version "0.1-SNAPSHOT"
  :author "Eitarow Fukamachi"
  :license "LLGPL"
  :depends-on (:cl-annot
               :alexandria
               :closer-mop
               :trivial-backtrace)
  :components ((:module "parser"
                :pathname "src/parser"
                :components
                ((:file "class" :depends-on ("util"))
                 (:file "util")
                 (:file "asdf" :depends-on ("class"))
                 (:file "parser" :depends-on ("class" "asdf" "util"))))))
