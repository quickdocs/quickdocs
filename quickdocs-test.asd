#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage quickdocs-test-asd
  (:use :cl :asdf))
(in-package :quickdocs-test-asd)

(defsystem quickdocs-test
  :depends-on (:quickdocs
               :cl-test-more)
  :components
  ((:module "t"
    :components
    ((:file "parse"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
