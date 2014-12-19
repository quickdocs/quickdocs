(in-package :cl-user)
(defpackage quickdocs-parser-asd
  (:use :cl :asdf))
(in-package :quickdocs-parser-asd)

(defsystem quickdocs-parser
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :description "The Quickdocs documentation parser."
  :depends-on (:cl-annot
               :cl-ppcre
               :cl-fad
               :yason
               :drakma
               :flexi-streams
               :alexandria
               :memoize
               :closer-mop
               :trivial-backtrace
               :bordeaux-threads)
  :components ((:module "parser"
                :components
                ((:file "class" :depends-on ("util"))
                 (:file "serialize" :depends-on ("util"))
                 (:file "asdf" :depends-on ("class"))
                 (:file "parser" :depends-on ("class" "asdf" "util" "serialize"))
                 (:file "util")))))
