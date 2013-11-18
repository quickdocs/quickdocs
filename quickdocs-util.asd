(in-package :cl-user)
(defpackage quickdocs-util-asd
  (:use :cl :asdf))
(in-package :quickdocs-util-asd)

(defsystem quickdocs-util
  :version "0.1"
  :author "Eitarow Fukamachi"
  :depends-on (:flexi-streams
               :alexandria)
  :components ((:module "util"
                :components
                ((:file "util")))))
