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
  :depends-on (:closer-mop
               :cl-ppcre
               :cl-annot)
  :components ((:module "parser"
                :pathname "src/parser"
                :components
                ((:file "class" :depends-on ("util"))
                 (:file "util")
                 (:file "asdf" :depends-on ("class"))
                 (:file "parser" :depends-on ("class" "asdf" "util"))))))
